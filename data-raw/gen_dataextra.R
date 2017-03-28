library(dplyr);library(tidyr)
library(devtools)

#### RAPA ####

rapa=read.table('data-raw/RAPA_Francois_data.csv',sep=";",header=T)
rapa=rapa %>% select(-idFlat, -Pot_position,-Pot_row,-Pot_column, -Flat_position_16LD) %>% dplyr::group_by(accessionid) %>% mutate_each(.,funs(mean))

print("The head lines of rapa:")
head(rapa,n=5)

devtools::use_data(rapa,overwrite = F)

#### Greenhouse data

patrice<-read.table('data-raw/1165_acc_patrice.tsv',fill = T,header=T)
head(patrice)

ft1001<-read.table('data-raw/1001genomes-FT10-FT16 and 1001genomes-accessions.tsv',fill=T,header=T)
head(ft1001)

print("The head lines of ft1001:")
head(ft1001,n=5)

devtools::use_data(ft1001,overwrite = F)

#### Data from 4 sites experiments of Fournier-level 2011 Science

fournier<-read.table('data-raw/fournier-level-2011-science.tsv',header=T)
head(fournier)

print("The head lines of fournier:")
head(fournier,n=5)

devtools::use_data(fournier,overwrite = F)
