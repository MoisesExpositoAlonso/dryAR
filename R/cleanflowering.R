#' Read and clean flowering time from experiment spatial grid raw data
#'
#' @param data long format flowering data
#' @details
#' A number of flags were used during flowering time accquisition. This function just considers everything that had these values as "dead" (or that never germinated). This is passed to make_data_flowering where it will be filtered

cleanflowering<-function(data=longform.rm){
dead=c("X","x","na","-",""," ","z","*","0","d","x","dead","-")

data$FT<-as.character.factor(data$FT)
data$FT[data$FT %in% dead] <-"dead"

unclear<-'?'
data$FT[data$FT %in% unclear] <-"unclear"

return(data)
}

