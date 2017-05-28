library(dplyr);library(tidyr)
library(devtools)

#### RAPA ####

rapa=read.table('data-raw/RAPA_Francois_data.csv',sep=";",header=T)
rapa=rapa %>% select(-idFlat, -Pot_position,-Pot_row,-Pot_column, -Flat_position_16LD) %>% dplyr::group_by(accessionid) %>% mutate_each(.,funs(mean))

print("The head lines of rapa:")
head(rapa,n=5)

devtools::use_data(rapa,overwrite = F)

#### drought data

droughtraw<-read.csv('data-raw/new_variables_traject_mean_pergenotype.csv')[,1:24] %>% select(-X,-latitude.x,-longitude.x,-kgroup)
colnames(droughtraw)<-gsub(gsub(colnames(droughtraw),pattern = 'X',replacement = 'day'),pattern = '.5',replacement = '')

droughtmodel<-read.csv('data-raw/m1diii_effects_29_parameters_prepared.csv') %>% select(-X)

drought<-merge(droughtraw,droughtmodel,by='id',all=T)

devtools::use_data(drought,overwrite = F)

#### all accessions from 1001

g1001<-read.csv('data-raw/1001_A.thaliana_genomes_oficial.csv') %>% select (-picture)

devtools::use_data(g1001,overwrite = F)


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


#### Slovak 2014 Plant Cell

root<-read.table('data-raw/Radka_rsms_ReTL_163_full_days_16traits.tsv',header=T, sep="\t")

root %>% filter(-SET) %>% mutate(id= ACC_ID )

devtools::use_data(root,overwrite = F)
