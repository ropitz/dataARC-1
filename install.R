list.of.packages <- c("R.utils","sf","rgdal","raster","rbioclim","stringr","stringi","proj4","data.table","FSA","plyr","wordcloud","mapview","KernSmooth","gdtools","FactoMineR","factoextra","sp","plyr","ggplot2","plotly","rgeos","randomForest","devtools","gdata","jsonlite")

install.packages(list.of.packages)

gdata::installXLSXsupport()

devtools::install_github("MoisesExpositoAlonso/rbioclim")