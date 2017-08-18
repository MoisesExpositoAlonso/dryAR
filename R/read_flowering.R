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
