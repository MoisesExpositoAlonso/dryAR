
genotypepositions<-function(location='tuebingen'){

thefile <-paste0("data-raw/GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-",toupper(location),"-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv")
ind <-read.table(thefile,sep="\t",header = T,stringsAsFactors = F)

ind<- rename(ind,water=water_type,qp=traynumber,row=rowcoor,col=colcoor,rep=replicate,indpop=rep_type) %>% mutate(row=tolower(row)) %>% mutate(pos= paste0(row,col))
head(ind)

unique(ind$othergeno)
unique(ind$otherpos)

badflags<- c("maybe","bad")
ind <- subset(ind,!(ind$quality %in% badflags)  )


for ( line in nrow(ind) ){
  theline=ind[line,]
  if (ind[line,'otherpos'] !=0){
    ind[line,'pos'] <- tolower(ind[line,'otherpos'])
  }
  if (ind[line,'othergeno'] !=0){
    ind[line,' id'] <- tolower(ind[line,'othergeno'])
  }
}


ind<-select(ind,-quality, -otherpos, -othergeno)   ### THIS PROBABLY NOT NECESSARY

return(data.frame(ind))

}

genotypepositions_two<-function(){}

# head(genotypepositions())
