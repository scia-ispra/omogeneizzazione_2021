---
title: Anomalie annuali di temperatura
output:
---

rm(list=objects())
library("tidyverse")


PARAM<-c("tmax","tmin")[2]

list.files(pattern =glue::glue("^anom_{PARAM}.+"))->ffile
ffile[grep("acmant",ffile)]->file_old
ffile[grep(glue::glue("cluster_{PARAM}"),ffile)]->file_cluster_param
ffile[grep(glue::glue("cluster_tmean"),ffile)]->file_cluster_tmean


read_delim(file_old,delim=";",col_names = TRUE) %>%
  mutate(tipo="old")->dati_old

read_delim(file_cluster_param,delim=";",col_names = TRUE) %>%
  mutate(tipo="cluster_param")->dati_cluster_param


read_delim(file_cluster_tmean,delim=";",col_names = TRUE) %>%
  mutate(tipo="cluster_tmean")->dati_cluster_tmean

bind_rows(dati_old,dati_cluster_param,dati_cluster_tmean) %>%
  dplyr::select(-anomAnnuale)->dati


ggplot(data=dati,aes(x=yy,y=anomAnnualePesata))+
  geom_line(aes(group=tipo,color=tipo))
