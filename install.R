list.of.packages <- c("R.utils","sf","rgdal","raster","rbioclim","stringr","stringi","proj4","data.table","FSA","plyr","wordcloud","mapview","KernSmooth","gdtools","FactoMineR","factoextra","sp","plyr","ggplot2","plotly","rgeos","randomForest","devtools","gdata","jsonlite","knitr","tibble","dplyr","tidyr","purrr")

install.packages(list.of.packages)

gdata::installXLSXsupport()

devtools::install_github("MoisesExpositoAlonso/rbioclim")


library(gdata) # for the trim function
library(jsonlite)
library(knitr)
library(stringr)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
