asmydate<-function(thedate,location='tuebingen'){
tmp<-as.numeric(thedate)
tmp<-ifelse( !is.na(tmp), paste0('2016-',substr(tmp,start=1,stop=1),'-',moiR::substrRight(tmp,2 ) ), NA)
mydate<-as.Date(tmp ,format= "%Y-%m-%d") - as.Date(sowingday()[[location]] )
return(mydate)
}
