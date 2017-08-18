read_count <- function(location="madrid"){

thefile=paste0('data-raw/',substr(location,1,3),'countharvest.csv')

har<-read.csv(thefile,header=T ,stringsAsFactors = F )
har$site=location
return(har)
}



read_labels <- function(location="madrid"){

thefile=paste0('data-raw/',substr(location,1,3),'harvest_image_index.csv')

har<-read.csv(thefile,header=T ,stringsAsFactors = F)
har$site=location
return(har)
}

################################################################################

correct_labels<-function(d){

  # d[d$site=='tuebingen',] <- .correct_tuebingen_labels(d[d$site=='tuebingen',]) # old code, this has problems with indexes
  # d[d$site=='madrid',] <- .correct_madrid_labels(d[d$site=='madrid',])

  message('correcting tuebingen...')
  tmp1=.correct_tuebingen_labels(d[d$site=='tuebingen',])
  message('correcting madrid...')
  tmp2= .correct_madrid_labels(d[d$site=='madrid',])


return(rbind(tmp1,tmp2))

}

.correct_tuebingen_labels<-function(d){

  ## the collected in the diary
  tocorrect<-read.table(sep='\t', 'data-raw/correct_tueharvest_fromdiary.tsv',header=T)
  tocorrect$position <- tolower(tocorrect$position)

  # not elegant, but better to assume these plants are dead than assign the wrong genotype
  message('considering dead all plants with errors, eliminating from dataset')
  d= filter(d , !(paste(tray,pos,sep='_') %in%   paste(tocorrect$qp,tocorrect$position,sep='_')) )

  ## change pop and ind to p and i codes
  d$indpop <- ifelse(d$indpop=='ind', 'i', 'p')

return(d)
}

.correct_madrid_labels<-function(d){

  d[which(d$pos =="b3 o b5"), 'pos']  <- 'b5' # this is B5, confirmed manually
  d$num[d$indpop == 'i']<-NA
  d$num[d$num == 'missin']<-(-9)
  d$num[d$num == '']<-(-9)

return(d)
}
