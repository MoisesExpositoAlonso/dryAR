library(dplyr);library(tidyr)
library(devtools)

#load_all("~/mexposito/moiR/")
library(moiR)

load_all(".")
# library(field)

##### Genotype and positions ####

repmad<-genotypepositions("madrid") %>% mutate(site='madrid')
reptue<-genotypepositions("tuebingen") %>% mutate(site='tuebingen')

genoreps=rbind(repmad,reptue)

print('This is the head/tail of genoreps:')
head(genoreps,n=5)
tail(genoreps,n=5)

# devtools::use_data(genoreps,overwrite = T)
devtools::use_data(genoreps,overwrite = F)
