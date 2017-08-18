library(dplyr);library(tidyr)
library(devtools)

load_all("~/mexposito/moiR/")
# library(moiR)

load_all(".")
# library(field)

#### Get the datas

data(acc)
data(flowering)
data(harvest)
data(veg)

dim(flowering)
dim(harvest)
dim(genoreps)
dim(veg)

head(flowering)
head(harvest)
head(veg)

#### master dataset ####

dupcol= c(colnames(flowering),colnames(harvest)) [ duplicated(c(colnames(flowering),colnames(harvest))) ]
c("qp","pos","site",'rep','trayid','water', 'indpop','id')
field <- acc %>%
            merge.data.frame(
              .,flowering,by="id" , all.y=T # because I want to keep all the replicates from field data
            ) %>%
            merge(x=.,y= harvest, by= dupcol, all.x=T) %>% # because want to keep also flowering that did not produce fruit
            merge(., veg, by=dupcol)


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

