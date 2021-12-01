rm(list=objects())
library("tidyverse")
library("daff")
library("seplyr")


annoF<-2020
basename(getwd())->PARAM
as.integer(annoF)-1->annoPrecedente

list.files(pattern=glue::glue("anom_{PARAM}_1961_{annoF}.+csv$"))->file_new
if(!length(file_new)) stop("Generare il file delle anomalie italiane per usare questo script!")
if(length(file_new)>1) stop("Troppi file, cosa succede?")
read_delim(file_new,delim=";",col_names=TRUE,col_types=cols(.default=col_double())) %>%
  seplyr::rename_se(c(glue::glue("anom_{annoF}"):="anom"))->dati_new


list.files(pattern=glue::glue("anom_{PARAM}_1961_{annoPrecedente}.+csv$"))->file_old
if(!length(file_old)) stop("Inserire nella directory il file delle anomalie italiane dello scorso anno!")
if(length(file_old)>1) stop("Troppi file, cosa succede?")
read_delim(file_old,delim=";",col_names=TRUE,col_types=cols(.default=col_double())) %>%
  seplyr::rename_se(c(glue::glue("anom_{annoPrecedente}"):="anom"))->dati_old

daff::diff_data(dati_new,dati_old)->mydiff


titolo<-glue::glue("Differenze tra anno {annoF-1} e {annoF} per indicatore: {PARAM}")
daff::render_diff(mydiff,use.DataTables = TRUE,fragment = FALSE,pretty = TRUE,title = titolo,file = glue::glue("cosa_cambia_{PARAM}.html"))
