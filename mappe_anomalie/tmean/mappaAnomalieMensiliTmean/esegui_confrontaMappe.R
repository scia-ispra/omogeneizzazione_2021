rm(list=objects())
library("rmarkdown")
library("stringr")

mesi<-str_pad(1:12,pad = "0",width = 2,side = "left")
purrr::walk(mesi,.f=function(.m){
  
  render("confrontaMappe.Rmd",params = list(mese=.m),output_file = glue::glue("confrontaMappe{.m}.html"))
  
})