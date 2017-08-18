#' Make relative fitness fro Genome Wide Hetterogeneous Selection GWHS
#'
#' @param f fitness vector
#' @param type what type of relative fitness is wanted
#' @param log logical
#'
#' @details
#'

relative <- function(f){
  tmp<-f / mean(f, na.rm=T)
  # tmp[is.na(tmp)]<-mean(tmp,na.rm=T)
return(tmp)
}
