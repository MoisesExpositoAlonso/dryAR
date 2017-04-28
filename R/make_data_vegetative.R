#' Read and clean data of vegetative plant information from image analysis outputs
#'
#' Returns a long format data frame with information on genotypes, replicates, blocks and
#' flowering data with its associated quality information (see details).
#'
#' @param location "madrid" or "tuebingen"
#' @details
#'
#' @seealso \url{github.com/MoisesExpositoAlonso/hitfruit} and \url{github.com/MoisesExpositoAlonso/hippo} for image processing analyses
#' @return Data frame of green rosette area of a specific location.
#' @export

make_data_vegetative <- function(location="madrid"){

thefile=paste0('data-raw/',substr(location,1,3),'countveggy.csv')

dat<-read.csv(thefile,header=T )
dat$site=location
dat=dplyr::select(dat,-qpot)
dat=dplyr::mutate(dat,pos=tolower(pos))
return(dat)
}

