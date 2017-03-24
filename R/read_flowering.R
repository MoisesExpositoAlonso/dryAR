#' @export

read_flowering <- function(location="tuebingen"){
if(is.null(location)){stop("Error: you can specify either tuebingen or madrid")}
if(location=="tuebingen"){
thefile <-"data-raw/Flowering_pheno_Madrid.xlsx-combined.tsv" }
if(location=="madrid"){
thefile <-"data-raw/Flowering_pheno_Tuebingen-combined.tsv" }

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
