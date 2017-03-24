#' Read and clean flowering time from experiment spatial grid raw data
#'
#' @param location madrid or tuebingen
#'
#' @return Data frame of flowering times of a specific location.
#' @export

refresh_data_flowering <- function(location){
# location='madrid'
## flowering time loading
flo=field::read_flowering(location)
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
                       row= fn(f.r),
                       col= fn(f.c),
                       pos= fc(tc),
                       FT=c(as.matrix(flo)) )

qpmap<- apply(longform,1,FUN=function(x) qp[ fn(x[1]) , fn(x[2]) ] )
longform$qp <- qpmap

# long form without the NAs
# remove the coreners
traycoord=traycoordinates()[traycoordinates()!="x"]
longform.rm<-subset(longform, (longform$pos %in% traycoord) )

# remove dead ones
longform.rm<-cleanflowering(longform.rm)
unique(longform.rm$FT)

# make a quality column
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
ind<-genotypepositions(location)
names(ind)
head(ind)

# Merge
ind$qp<-as.character(ind$qp)
ind$pos<-as.character(ind$pos)
longform.rm$qp<-as.character(longform.rm$qp)
longform.rm$pos<-as.character(longform.rm$pos)
dim(ind)
dim(longform.rm)

longform.rm$qp
ind$qp
as.character(ind$qp)[1]

indadd<- merge(x=ind, y=longform.rm, by=c('qp',"pos"), all.x = T  ) ### This will generate NAs where no flowering was measured.
head(indadd)
tail(indadd)

# remove bad quality ones
badflags<- c("maybe","bad")
indadd.rm <- subset(indadd,!(indadd$quality %in% badflags) | !(indadd$otherpos %in% badflags) ) %>% select(.,-quality, -otherpos, -othergeno)   ### THIS PROBABLY NOT NECESSARY

save(file=paste("data/flowering_",location,".RData"), indadd.rm)
write.tsv(file=paste("data/flowering_",location,".tsv"), indadd.rm)

return(indadd.rm)
}

