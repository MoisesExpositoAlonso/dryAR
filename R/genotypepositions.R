#' Get the genotype positions in blocks, trays, and within-tray of the two field experiments
#'
#' @param data long format flowering data
#' @param QC logical whether quality filter should be done or not
#' @details
#' Reads the tsv file of the sown genotypes and replicate id's. Includes quality control from the errors
#' spotted in situ during the sowing day.
#'

genotypepositions<-function(location='tuebingen',QC=TRUE){

thefile <-paste0("data-raw/GENOTYPES_REPLICATES_TREATMENTS_POSITIONS-",toupper(location),"-2015-9-3_Experiment_1001g_field_design_acc_trays.csv.tsv")
ind <-read.table(thefile,sep="\t",header = T,stringsAsFactors = F)

ind<- rename(ind,water=water_type,qp=traynumber,row=rowcoor,col=colcoor,rep=replicate,indpop=rep_type) %>% mutate(row=tolower(row)) %>% mutate(pos= paste0(row,col))
head(ind)

unique(ind$othergeno)
unique(ind$otherpos)

which(ind$otherpos !=0)

if(QC==TRUE){
badflags<- c("maybe","bad")
ind <- subset(ind,!(ind$quality %in% badflags)  )
}

for ( line in 1:nrow(ind) ){
  # message(line)
  if(ind[line,'otherpos'] !=0){
    ind[line,'pos'] <- tolower(ind[line,'otherpos'])
  }
  if (ind[line,'othergeno'] !=0){
    ind[line,' id'] <- tolower(ind[line,'othergeno'])
  }
}

subset(ind, ind$otherpos!=0)

ind<-select(ind,-quality, -otherpos, -othergeno, -row, -col)   ### THIS PROBABLY NOT NECESSARY

return(data.frame(ind))

}
