#' Proportion of variance of a random factor from a MCMCglmm output object
#'
#' @param mcmcglmmobject
#' @param randomname
#'
#' @return
#' @export
#'
#' @examples
h2MCMC=function(mcmcglmmobject, randomname){

allvariance=apply(mcmcglmmobject$VCV, 1, sum)
posterior<-mcmcglmmobject$VCV[,randomname]/ allvariance

return(list(Mode=posterior.mode(posterior),HPD=HPDinterval(posterior,0.95)))

}
