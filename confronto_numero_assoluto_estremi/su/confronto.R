

rm(list=objects())
library("tidyverse")
library("echarts4r")
library("guido")

leggi_file("ana_.+csv",delim=";",col_names = TRUE,output = "_dfr",col_types = cols(.default = col_character()),path="../",full.names=TRUE)->ana


read_delim("risultati_su.csv",delim=";",col_names = TRUE) %>%
  gather(key="id",value="homog",-yy)->hdati

read_delim("raw_risultati_su.csv",delim=";",col_names = TRUE) %>%
  gather(key="id",value="raw",-yy)->rdati


left_join(hdati,rdati) %>%
  mutate(decade=str_sub(yy,1,3))->df

left_join(df,ana %>% dplyr::select(-yy))->df


# ggplot(data=df,aes(x=homog,y=raw))+
#   geom_point(aes(color=cluster))+
#   geom_smooth(se=FALSE)+
#   geom_abline(slope=1,intercept=0,color="red")+
#   facet_wrap(~decade)+
#   scale_color_discrete()+
#   theme_bw()

sort(unique(df$decade))->v_decadi
v_decadi[length(v_decadi)]->ultima_decade

purrr::map(v_decadi,.f=function(DECADE){

  e_charts(df %>% filter(decade==DECADE) %>% mutate(cluster=as.character(cluster)),x = homog,elementId = DECADE) %>%
    e_scatter(serie=raw,symbol_size = 8) %>%
    e_lm(raw~homog) %>%
    e_line(serie=homog,color="black",lineStyle="solid") %>%
    e_tooltip(tirgger="axis") %>%
    e_title(text=paste0("Decade: ",DECADE,".")) %>%
    e_legend(show=FALSE) %>%
    e_x_axis(name="homog") %>%
    e_y_axis(name="raw")->grafico
  
  if(DECADE==ultima_decade){
    grafico %>%
      e_connect(base::setdiff(v_decadi,ultima_decade))
  }
  
  grafico
    
  
})->listaScatter

e_arrange(listaScatter[1:length(v_decadi)],rows=4,width="xs",cols=2)

  

v_cluster<-unique(df$cluster)
v_cluster[length(v_cluster)]->ultimo_cluster

purrr::map(v_cluster,.f=function(CLUSTER){
  
  e_charts(df %>% filter(cluster==CLUSTER),x = homog,elementId = CLUSTER) %>%
    e_scatter(serie=raw,symbol_size = 8) %>%
    e_lm(raw~homog) %>%
    e_line(serie=homog,color="black",lineStyle="solid") %>%
    e_tooltip(tirgger="axis") %>%
    e_title(text=paste0("Cluster: ",CLUSTER,".")) %>%
    e_legend(show=FALSE) %>%
    e_x_axis(name="homog") %>%
    e_y_axis(name="raw")->grafico
  
  if(CLUSTER==ultimo_cluster){
    grafico %>%
      e_connect(base::setdiff(v_cluster,ultimo_cluster))
  }
  
  grafico
  
  
})->listaScatter

e_arrange(listaScatter[1:length(v_cluster)],rows=4,width="xs",cols=2)
