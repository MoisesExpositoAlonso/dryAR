library(dplyr);library(tidyr)
library(devtools)

load_all("~/mexposito/moiR/")
# library(moiR)
load_all(".")
# library(field)

## read harvesting info

tuev <- make_data_vegetative(location="tuebingen")
madv <- make_data_vegetative(location="madrid")
veggy<-rbind(madv,tuev)

head(veggy)
tail(veggy)
dim(veggy)

# merge

data("genoreps")
head(genoreps)

veggy= merge(veggy,genoreps, by=c('qp','pos','site'),all.y=T)   ### NOTE ALSO THAT CAN BE SEVERAL PICTURES FOR A SINGLE POT!
head(veggy)

# correct two folders with a non-standard name
veggy$folder=gsub(veggy$folder,pattern = '2015_12_09y10', replacement = '2015_12_09',fixed=T) # to homogenize
veggy$folder=gsub(veggy$folder,pattern = '2015_11_17_incompleto', replacement = '2015_11_17',fixed = T) # to homogenize

# remove image information
veggy= veggy %>% mutate( pathimage= paste(pathimage,folder,image,sep='/') ) %>%
                mutate(day=as.Date(folder,format= "%Y_%m_%d")) %>%
                select(-folder, -image)
head(veggy)

# save as package database
veggyraw=veggy
devtools::use_data(veggyraw,overwrite = T)

