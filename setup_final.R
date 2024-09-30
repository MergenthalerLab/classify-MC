initialize_ = F
source_ = T

if(initialize_){
  install.packages("Rcpp", dependencies = TRUE)
  install.packages('pillar')
  install.packages("installr")
  install.packages("dplyr", dependencies = TRUE)
  install.packages("tidyr")
  install.packages("stringr")
  install.packages("lubridate")
  install.packages("ggplot2")
  install.packages("leaflet")
  install.packages("ggmap")
  install.packages("data.table")
  install.packages("readr")
  install.packages("DBI")
  install.packages("odbc")
  install.packages("dbplyr")
  install.packages("purrr")
  install.packages("RMySQL")
  install.packages("ggthemes")
  install.packages("forcats")
  install.packages('readxl')
  install.packages('tibble')
  install.packages('mice')
  install.packages('partykit')
  install.packages('rgdal')
  install.packages('sp')
  install.packages('scales')
  install.packages('glmnet')
  install.packages('knitr')
  install.packages('rmarkdown')
  install.packages('ROCR')
  install.packages('purrr')
  install.packages('ggrepel')
  install.packages('scales')
  install.packages('gt')
  install.packages('mlr')
  install.packages('glue')
  install.packages('glmnet')
  install.packages('htmltools', version = '0.5.2')
  install.packages("randomForest")
  install.packages("party")
  install.packages("tensorflow")
  #install.packages("keras")
  install.packages("patchwork")
  install.packages("svglite")
  

}

if(source_){
  library(plyr)
  library(pillar)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(leaflet)
  library(ggmap)
  library(data.table)
  library(readr)
  library(DBI)
  library(odbc)
  library(dbplyr)
  library(RMySQL)
  library(ggthemes)
  library(purrr)
  library(forcats)
  library(readxl)
  library(tibble)
  library(mice)
  library(partykit)
  library(broom)
  library(rgdal)
  library(sp)
  library(scales)
  library(dplyr)
  #library(plotrix)
  library(glmnet)
  library(knitr)
  library(rmarkdown)
  library(ROCR)
  library(purrr)
  library(ggrepel)
  library(scales)
  library(forcats)
  library(glue)
  library(gt)
  library(mlr)
  library(pROC)
  library(glue)
  library(glmnet)
  library(htmltools)
  library(randomForest)
  library(party)
  library(patchwork)
  library(svglite)
  
  #library(tensorflow)
  #reticulate::install_miniconda()
  #install_tensorflow()
  mutate = dplyr::mutate
  summarize = dplyr::summarize
}

#load all functions:
#manipulates dates so that we have year, month, day in one col each. Te
date_manip = function(df){
  if('Gebdat' %in% names(df)){
    df= mutate(df, year = substr(Gebdat, 7,10), month = substr(Gebdat, 4,5), day = substr(Gebdat, 1,2))
  }else{
    df= mutate(df, year = substr(Geburtsdatum, 1,4), month = substr(Geburtsdatum, 6,7), day = substr(Geburtsdatum, 9,10))
  }
}



quantile_cut = function(x, n=5, digits=2, labels = NULL, breaks = 0:n/n) {
  cut_points = unique(quantile(x, breaks, na.rm=T))
  print(breaks)
  print(cut_points)
  if(is.null(labels)) {
    labels = paste(round(cut_points[1:length(cut_points) - 1], digits), " - ", round(cut_points[2:length(cut_points)], digits), sep="")
  }
  labels = head(labels, length(cut_points) - 1)
  
  cut(x, cut_points, include.lowest = T, labels = labels, ordered_result = T)
}


`%grin%` <- function(str, expressions) {
  wrapped = paste0("(", expressions, ")")
  pattern = paste(wrapped, collapse = "|")
  return(grepl(pattern, str, ignore.case = T))
}


theme_MG = function (base_size = 14, base_family = "", base_line_size = base_size/22, 
          base_rect_size = base_size/22) 
{
  theme_bw(base_size = base_size, base_family = base_family, 
           base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(axis.ticks = element_blank(), legend.background = element_blank(), 
          legend.key = element_blank(), panel.background = element_blank(), 
          panel.border = element_blank(), strip.background = element_blank(), 
          plot.background = element_blank(), axis.text.x=element_text(angle=90), complete = TRUE)
}


