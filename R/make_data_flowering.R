#' Read and clean flowering time from the experiment's raw data
#'
#' Returns a long format data frame with information on genotypes, replicates, blocks and
#' flowering data with its associated quality information (see details).
#'
#' @param location "madrid" or "tuebingen"
#' @details
#'
#' The function generates 3 additional columns that are added to the genotype
#' positions \code{\link{genotypepositions}}
#'
#' FT.q indicates the quality of the pot. Particularly every pot that does not have a plant, or other
#' symbol of death, would be marked as 0. When it was not clear, for example, when during monitoring
#' we would see a pot with a blue pin but not flowering date, we would either estimate the date or
#' give the date when we found this error. When this was particularly unclear, a -9 is assigned
#' (from comments in the google spreadsheet). Finally, to all those pots that could be assigned
#'  a flowering date, the number is 1.
#'
#' FT.date is precissely that date
#'
#' FT.dif is the value from the date of sowing in the field and the flowering date. Perhaps more
#' informative could be the date difference between germination time and flowering time, but
#' germination time has to be assigned a posteriori from image analysis.
#'
#' NOTE: When merging flowering data information, I kept flag all.x=T for the genotype locations.
#' This genotype/replicate positions list contain only positions that were correctly sown
#' so no data of floweirng time at those
#'
#' @seealso \code{\link{cleanflowering}} for the possible flags of dead plants
#' and \code{\link{genotypepositions}} for the original genotype/replicate/block list.
#' @return Data frame of flowering times of a specific location.
#' @export


make_data_flowering <- function(location){
# location='madrid'
# location='tuebingen'
## flowering time loading
flo=read_flowering(location)
# flo=read_flowering('tuebingen')
# flo=read_flowering('madrid')


# make column index
f.c<- makecols()
# make row index
f.r<- makerows()
# make coordinate tray index
tc <- traycoordinates_tunnel()
# get the qp
qp <- quickpots()

# build together long format
longform <- data.frame(
                       row= moiR::fn(f.r),
                       col= moiR::fn(f.c),
                       pos= moiR::fc(tc),
                       FT=c(as.matrix(flo)) )

qpmap<- apply(longform,1,FUN=function(x) qp[ fn(x[1]) , fn(x[2]) ] )
longform$qp <- qpmap

# long form without the NAs
# remove the coreners
traycoord=traycoordinates()[traycoordinates()!="x"]
longform.rm<-subset(longform, (longform$pos %in% traycoord) )

# remove dead flags ones
longform.rm<-cleanflowering(longform.rm)
unique(longform.rm$FT)

# make a quality column with the found flags
longform.rm
head(longform.rm)

longform.rm$FT.q=1
longform.rm$FT.q[longform.rm$FT=="dead"]=0
longform.rm$FT.q[longform.rm$FT=="unclear"]=-9

# make column date
longform.rm$FT.date=as.numeric(longform.rm$FT)
longform.rm$FT.date=ifelse( !is.na(longform.rm$FT.date), paste0('2016-',substr(longform.rm$FT.date,start=1,stop=1),'-',moiR::substrRight(longform.rm$FT.date,2 ) ), NA)
longform.rm$FT.date=as.Date(longform.rm$FT.date ,format= "%Y-%m-%d")
longform.rm$FT.dif=as.numeric( as.Date(longform.rm$FT.date)- as.Date(sowingday()[[location]] ) )

longform.rm=dplyr::select(longform.rm,-FT,-row,-col)

# index genotypes and replicates
genoreps<-genotypepositions(location)
names(genoreps)
head(genoreps)

# Merge with the genotypes that were sown
genoreps$qp<-as.character(genoreps$qp)
genoreps$pos<-as.character(genoreps$pos)
longform.rm$qp<-as.character(longform.rm$qp)
longform.rm$pos<-as.character(longform.rm$pos)
dim(genoreps)
dim(longform.rm)

unique(longform.rm$FT.q)

flor<- merge(x=genoreps, y=longform.rm, by=c('qp',"pos"), all.x = T  ) ### This will generate NAs where no flowering was measured.
dim(flor) # dimensions are smaller than what the data of flowering has because we restrict to those locations that we are sure they were properly sown.
head(flor)
tail(flor)
unique(flor$FT.q)

# remove bad quality ones

save(file=paste("data/flowering_",location,".RData"), flor)
write.tsv(file=paste("data/flowering_",location,".tsv"), flor)

return(flor)
}



#' Read and clean flowering time from experiment spatial grid raw data
#'
#' @param data long format flowering data
#' @details
#' A number of flags were used during flowering time accquisition. This function just considers everything that had these values as "dead" (or that never germinated). This is passed to make_data_flowering where it will be filtered

cleanflowering<-function(data=longform.rm){
dead=c("X","x","na","-",""," ","z","*","0","d","x","dead","-")

data$FT<-as.character.factor(data$FT)
data$FT[data$FT %in% dead] <-"dead"

unclear<-'?'
data$FT[data$FT %in% unclear] <-"unclear"

return(data)
}



#' Read the flowering time raw data
#'
#' @param location madrid or tuebingen
#' @details
#' reads/returns raw data of flowering time dates in the spatial position of the field experiment.

read_flowering <- function(location="tuebingen",path='data-raw'){

if(is.null(location)){stop("Error: you can specify either tuebingen or madrid")}

if(location=="madrid"){
  thefile <-paste0(path,"/Flowering_pheno_Madrid.xlsx-combined.tsv")
  # thefile <-"Flowering_pheno_Madrid.xlsx-combined.tsv"
  print(thefile)
}

if(location=="tuebingen"){
  thefile <-paste0(path,"/Flowering_pheno_Tuebingen-combined.tsv")
}

fmad<-read.table(thefile,fill=T,sep="\t",stringsAsFactors = F)
head(fmad,n = 20)
tail(fmad,n = 20)
fmad2<-fmad[,-11]
fmad2<-cbind(fmad2[which(fmad[,11]=="row1" ) ,] ,
             fmad2[which(fmad[,11]=="row2" ) ,] ,
             fmad2[which(fmad[,11]=="row3" ) ,] ,
             fmad2[which(fmad[,11]=="row4" ) ,]
             )
return(fmad2)
}
