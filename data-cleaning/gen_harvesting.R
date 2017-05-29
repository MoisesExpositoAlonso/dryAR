library(dplyr);library(tidyr)
library(devtools)

load_all("~/moiR")
# library(moiR)
load_all(".")
# library(field)


####################################################################################
## read harvesting info

# tueh <- make_data_harvest(location="tuebingen")
# madh <- make_data_harvest(location="madrid")
# harvest<-rbind(madh,tueh) #### <<- need to change when tuebingen is annotated
# harvest<-(madh) #### <<- need to change when tuebingen is annotated
# harvestraw<-harvest
# devtools::use_data(harvestraw,overwrite = F)

data(harvestraw)
harvest=harvestraw
head(harvest)
tail(harvest)
dim(harvest)

# merge

data("genoreps")
head(genoreps)

harvest= merge(harvest,genoreps, by=c('qp','pos','site'),all.y=T)   ### NOTE ALSO THAT CAN BE SEVERAL PICTURES FOR A SINGLE POT!
head(harvest)

# put in the number column NA when it is an individual replicate
harvest$num[harvest$indpop == 'i']<-NA

harvest$num[harvest$num == 'missin']<-(-9)

# add quality flag
harvest$Harv.q <- 0
harvest$Harv.q[ !is.na(harvest$pathimage) ] <-1

unique(harvest$Harv.q)
unique(harvest$water)

head(harvest)
tail(harvest)
dim(harvest)

## save
print('The first lines of data index of harvest:')
head(harvest,n=5)
print("Rows without data:")
table(is.na(harvest$totarea))

devtools::use_data(harvest,overwrite = T)
