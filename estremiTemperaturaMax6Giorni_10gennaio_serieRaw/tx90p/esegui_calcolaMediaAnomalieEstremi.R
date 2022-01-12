rm(list=objects())

basename(getwd())->indice
rmarkdown::render(input ="calcolaMediaAnomalieEstremi.Rmd",output_file=paste0(indice,".html"))
