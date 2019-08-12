library(dplyr)
load("data/gbif.rda")

gbif_back<-gbif
gbif$latround<-round(gbif$lat,digits=2)
gbif$lonround<-round(gbif$lon,digits=2)

gbif<-dplyr::filter(gbif, lon> -25, lat>15)

unique(gbif$latround) %>% summary
unique(gbif$lonround) %>% summary