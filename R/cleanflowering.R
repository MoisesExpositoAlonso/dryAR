
cleanflowering<-function(data=longform.rm){
dead=c("X","x","na","-",""," ","z","*","0","d","x","dead","-")

data$FT<-as.character.factor(data$FT)
data$FT[data$FT %in% dead] <-"dead"

unclear<-'?'
data$FT[data$FT %in% unclear] <-"unclear"

return(data)
}

confusion<-'?'
