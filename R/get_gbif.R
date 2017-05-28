
get_gbif<-function(genus = "Arabidopsis",species = "thaliana"){

gbif.d=dismo::gbif(genus = "Arabidopsis",species = "thaliana",geo=T,removeZeros=T)
gbif=na.omit(gbif.d[,c("lat","lon")])

devtools::use_data(gbif,overwrite = TRUE)
return(gbif)
}