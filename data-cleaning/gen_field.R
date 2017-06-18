library(dplyr);library(tidyr)
library(devtools)

load_all("~/moiR/")
# library(moiR)

load_all(".")
# library(field)

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

# field <-
  acc %>%
            merge.data.frame(
              .,flowering,by="id" , all=T ) %>% # because I want to keep all the replicates from field data
            merge(x=.,y= veg, by= dupcol, all=T) %>%
            merge(x=.,y= harvest, by= dupcol, all=T)
    # tail
    head


dim(field)
head(field)

print('The first lines of field:')
print(head(field,5))

devtools::use_data(field,overwrite = TRUE)


#### mastergeno dataset ####

# data(acc)
#
# dm<-merge(fou,by.x = 'ecotype_id',acc,'id')
# nrow(dm)

