# ---
# title: Read the harvesting data
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# ---
message('--------------------------------------------------')
message('Generating genotype, replicates and locations data')
message('--------------------------------------------------')

wannaoverwrite=T


library(dplyr)
library(tidyr)
library(devtools)
load_all("~/mexposito/moiR/")
load_all(".")

##### Genotype and positions ####

repmad<-genotypepositions("madrid") %>% mutate(site='madrid')
reptue<-genotypepositions("tuebingen") %>% mutate(site='tuebingen')

genoreps=rbind(repmad,reptue)

print('This is the head/tail of genoreps:')
print(head(genoreps,n=5))
print(tail(genoreps,n=5))

# devtools::use_data(genoreps,overwrite = T)
devtools::use_data(genoreps,overwrite = wannaoverwrite)
