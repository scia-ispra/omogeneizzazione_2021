#30 novembre 2021
#Codice rivisto per le nuove serie omogeneizzate di temperatura, rivisti i criteri per la scelta delle serie da utilizzare per il calcolo dell'anomalia italiana
#
#Il problema degli estremi e' che l'anomalia viene calcolata su serie puntuali, senza effettuare alcuna spazializzazione.
rm(list=objects())
options(error=NULL)
library("tidyverse")
library("parallel")
library("chron")
library("climdex.pcic")
library("sf")
library("regioniItalia")
library("climatologici")
library("guido")
library("config")

Sys.setenv(R_CONFIG_ACTIVE = "default")
purrr::partial(.f=config::get,file="estremi.yml")->eget

#quale parametro?
PARAM<-eget("param")

#calcolare indici a livello mensile?
CALCOLA.MENSILI<-eget("calcola.mensili")

annoI<-eget("annoI")
annoF<-eget("annoF")

creaCalendario(annoI,annoF) %>%
  dplyr::select(yy) %>%
  distinct(yy)->calendario_anni

nrow(calendario_anni)*eget("percentuale.anni.validi")->numeroAnniValidi


#leggo dati giornalieri di tmax e tmin, interseco i codici stazione e utilizzo solo le stazioni comuni a Tmax e Tmin per il calcolo degli estremi
read_delim("Tmax.csv",delim=";",col_names = TRUE,col_types =YYMMDD_TYPE)->temp_tmax
read_delim("Tmin.csv",delim=";",col_names = TRUE,col_types =YYMMDD_TYPE)->temp_tmin

base::intersect(names(temp_tmax),names(temp_tmin))->codici_comuni
stopifnot(length(codici_comuni)!=0)

#file dati con le serie omogeneizzate
list.files(pattern=glue::glue("^{PARAM}.csv$"))->file.dati
stopifnot(length(file.dati)==1)

read_delim(file.dati,delim=";",col_names = TRUE,col_types =YYMMDD_TYPE) %>% 
  filter(yy>=annoI & yy<=annoF)->dati

dati[,names(dati) %in% codici_comuni]->dati

list.files(pattern = glue::glue("^anagrafica.{tolower(PARAM)}.csv$"))->file.ana
read_delim(file.ana,delim=";",col_names = TRUE)->ana

###################################################
#lista indici
if(PARAM=="Tmax"){
  indici<-c("txx","txn","tx10p","tx90p","su","wsdi")  
}else if(PARAM=="Tmin"){
  indici<-c("tnx","tnn","tn10p","tn90p","fd","tr","csdi")  
}else if(PARAM=="Prcp"){
  indici<-c("cdd","cwd","prcptot","r10mm","r20mm","r95ptot","r99ptot","rx1day","rx5day","sdii")
}else{
  stop("parametro non riconosciuto")
}
###################################################


#quali di questi sono mensili
indiciMensili<-c("rx1day","rx5day","tn10p","tn90p","tx10p","tx90p","tnn","tnx","txn","txx")

(dati %>% 
    mutate(calendario.pcic=as.PCICt(paste(yy,mm,dd,sep="-"),cal="gregorian")))[["calendario.pcic"]]->calendario

cpar<-tolower(PARAM)

purrr::map_dfc(indici,.f=function(indice){
  
  #crea directory con il nomde dell'indicatore
  cat(sprintf("ELABORAZIONE INDICI PER: %s\n",indice))  
  if(!dir.exists(indice)) try(dir.create(indice))  
  
  #gli indici in generale nn richiedono questa opzione, default la stringa è vuota
  opzione.stringa.climdex<-""    
  
  ###################################################
  #però se un indice è un indice calcolabile anche su base mensile e si vuole
  #calcolare su base mensile allora dobbiamo distinguire
  if(indice %in% indiciMensili){
    
    if(CALCOLA.MENSILI){
      #indici mensili
      opzione.stringa.climdex<-",freq=c('monthly')"
    }else{
      opzione.stringa.climdex<-",freq=c('annual')"
    }

  }    

  purrr::imap(dati %>%select(-yy,-mm,-dd) ,.f=function(.x,.y){
    

    #stringa: varia in base a cpar, fissato nel ciclo for su ff 
    stringa.climdex<-paste0("climdexInput.raw(",cpar,"=.x,",cpar,".dates=calendario,",
                                    "base.range=c(1961, 1990), max.missing.days=c(annual=15,monthly=6),min.base.data.fraction.present=0.1 )") 
                      
        #creazione oggetto climdex
        eval(parse(text=stringa.climdex))->ci
        
        #calcolo indice estremo: la stringa formata con paste0 crea ad esempio: climdex.wsdi
        # o climdex.txx(ci,freq="monthly") nel caso di indici con aggregazione mensile
        eval(parse(text=paste0("climdex.",indice,"(ci",opzione.stringa.climdex,")")))->out
        tibble(yy=names(out),x=out) %>% 
          mutate(yy=as.integer(yy))->annuale
        
        #verifica periodo 1961-1990: devo essere sicuro di avere almeno 24 anni validi nel periodo di riferimento
        #per il calcolo del climatologico
        annuale  %>%
          filter(yy>=1961 & yy<=1990) %>%
          filter(is.na(x))->subAnnuale
        
        nrow(subAnnuale)->numeroNA
        
        if(numeroNA>=7) return() #il climatologico deve avere al piu' 6 NA nel trentennio climatologico
        
        #gli ultimi anni (tre anni) della serie devono essere validi
        if(all(is.na(annuale$x[annuale$yy %in% (annoF:(annoF-2))]))) return()
        
        #per mantenere piu' serie possibili ed essere sicuri di avere serie sufficientemente "piene", calcoliamo il numero di anni
        #per ciascun decennio (escludendo l'ultimo decennio che e' appena iniziato). Eliminiamo le serie che per decennio hanno meno di 5
        #anni validi (siccome questo controllo viene dopo quello sul climatologico, sappiamo che il trentennio climatologico avrà cmq abbastanza dati)
        annuale %>%
          mutate(decennio=str_remove(as.character(yy),".$")) %>%
          filter(!is.na(x)) %>%
          group_by(decennio) %>%
          summarise(conteggioDatiPerDecennio=n()) %>%
          ungroup() %>%
          filter(decennio!="202")->conteggio
        
        sum(conteggio$conteggioDatiPerDecennio)->numeroRigheValidi
        
        #5 anni per decennio
        which(conteggio$conteggioDatiPerDecennio<5)->righe
        if(length(righe)) return()
        
        #Per alcuni indicatori escludiamo le serie fatte quasi solo di zeri
        which(annuale$x<0.01 & !is.na(annuale$x))->righeNulle
        if(length(righeNulle)> (numeroRigheValidi-10)) return()        
        
        #ok la serie e' valida
        names(annuale)[ncol(annuale)]<-.y
        
        annuale
  
  }) %>% purrr::compact(.)->lista.out
  
  if(!length(lista.out)) stop(glue::glue("Nessuna serie valida per l'indice {indice}"))
  purrr::reduce(lista.out,.f=left_join,.init=calendario_anni)->da_scrivere
  
  #questo serve per il calcolo delle anomalie
  write_delim(da_scrivere,file=paste0("./",indice,"/risultati_",indice,".csv"),col_names=TRUE,delim=";")
  
  #mappa delle stazioni selezionate
  ana %>%
    mutate(id=paste0(regione,"_",SiteID)) %>%
    filter(id %in% names(da_scrivere))->subAna
  
  write_delim(subAna,paste0("./",indice,"/ana_",indice,".csv"),delim=";",col_names=TRUE)
  
  subAna %>%
    st_as_sf(coords=c("Longitude","Latitude"),crs=4326)->puntiStazione

  st_transform(puntiStazione,crs=32632)->utm_puntiStazione
  
  #mappa
  pdf(paste0("./",indice,"/risultati_",indice,".pdf"),width=8,height=10)  
  plot(st_geometry(italia),main=glue::glue("Numero stazioni: {ncol(da_scrivere)-1}"))
  plot(st_geometry(utm_puntiStazione),add=TRUE,pch=21,color="red")
  dev.off()
  
  ncol(da_scrivere)-1
  
  
})->numeroStazioni #fine purrr::map su indice  

#sintesi del numero delle stazioni per indice
names(numeroStazioni)<-indici
write_delim(numeroStazioni,paste0("numeroStazioniEstremi",PARAM,".csv"),delim=";",col_names = TRUE)



