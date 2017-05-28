library(devtools)

#### Accessions list data ####
acc=read.table("data-raw/GENOTYPES_LIST-1001G_downsample_524_extrabyeye_GREENHOUSE _FIELD_EXPERIMENT_FINAL.csv.tsv",header = T)
print("The head lines of acc:")
head(acc,n=5)

devtools::use_data(acc)
