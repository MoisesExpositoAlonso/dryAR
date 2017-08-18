# ---
# title: Read the harvesting data
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# ---

message('-------------------------------------------------')
message('Generating accessions list')
message('-------------------------------------------------')

library(devtools)

### Get the two laboratory accessions
data("g1001")
filter(g1001, name=='Col-0')
filter(g1001, name=='Ler-1')

#### Accessions list data ####
acc=read.table("data-raw/GENOTYPES_LIST-1001G_downsample_524_extrabyeye_GREENHOUSE _FIELD_EXPERIMENT_FINAL.csv.tsv",header = T)

acc[grep('Col', acc$name) , ]<-c(6909,'Col-0', 'USA',  38.3,-92.3,NA)
acc[grep('Ler', acc$name) , ]<- c(6932 ,'Ler-1', 'GER',47.984 , 10.8719, NA)

print("The head lines of acc:")
print(head(acc,n=5))

devtools::use_data(acc,overwrite=T)
