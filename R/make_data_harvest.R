#' Read and clean data of harvested plants from the image analysis outputs
#'
#' Returns a long format data frame with information on genotypes, replicates, blocks and
#' flowering data with its associated quality information (see details).
#'
#' @param location "madrid" or "tuebingen"
#' @details
#'
#' @seealso \url{github.com/MoisesExpositoAlonso/hitfruit} and \url{github.com/MoisesExpositoAlonso/hippo} for image processing analyses
#' @return Data frame of harvesting info of a specific location.
#' @export

make_data_harvest <- function(location="madrid"){

thefile=paste0('data-raw/',substr(location,1,3),'countharvest.csv')

har<-read.csv(thefile,header=T )
har$site=location
har=rename(har,qp=tray)
return(har)
}

