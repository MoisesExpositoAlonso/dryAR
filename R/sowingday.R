sowingday<-function(site=NULL){
  thelist=list(tuebingen=as.Date("2015-10-22"),madrid=as.Date("2015-11-16"))

  if(is.null(site)){
    return(thelist)
  }
  else if(site=='tuebingen'){
  return(thelist$tuebingen)
  }
  else if(site=='madrid'){
  return(thelist$madrid)
  }
  else{stop("Need to provide a valid argument for *site*")}
}
