library(dplyr);library(tidyr)
library(devtools)

load_all("~/mexposito/moiR/")
# library(moiR)

load_all(".")
# library(field)

#### Get the datas

data(acc)
data(flowering)

#### master dataset ####

master<- merge(acc,flowering,by="id")
head(master)

devtools::use_data(master,overwrite = TRUE)


#### mastergeno dataset ####

# data(acc)
#
# dm<-merge(fou,by.x = 'ecotype_id',acc,'id')
# nrow(dm)

