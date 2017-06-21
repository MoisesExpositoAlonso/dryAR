library(dplyr);library(tidyr)
library(devtools)

load_all("~/moiR/")
# library(moiR)

load_all(".")
# library(field)

#### wannaoverwrite
wannaoverwrite=T

#### Get the datas

data(acc)
data(flowering)
data(veg)
data(harvest)

dim(flowering)
dim(harvest)
dim(genoreps)

head(acc)
head(flowering)
head(veg)
head(harvest)

#### master dataset ####

dupcol= c(colnames(flowering),colnames(harvest)) [ duplicated(c(colnames(flowering),colnames(harvest))) ]
c("qp","pos","site",'rep','trayid','water', 'indpop','id')

harvestinflorescence= harvest %>% filter(Harv.organ =='i')

dim(field)
names(field)
names(veg)

c(colnames(flowering),colnames(veg))[ duplicated(c(colnames(flowering),colnames(veg)))  ]
c(colnames(flowering),colnames(harvest)) [ duplicated(c(colnames(flowering),colnames(harvest))) ]

dim(veg)
veg

field <-
  acc %>%
    # because I want to keep all the replicates from field data
    merge.data.frame(
      .,flowering,by="id", all=T ) %>%
    # add the vegetative
    merge(x=.,y= veg, by= dupcol, all=T) %>%
    merge(x=.,y= harvestinflorescence, by= dupcol, all=T)
dim(field)



print('The dimensions and first/last lines of field:')
dim(field)
head(field)
tail(field)
names(field)

if(wannaoverwrite==T)
  devtools::use_data(field,overwrite = wannaoverwrite)

