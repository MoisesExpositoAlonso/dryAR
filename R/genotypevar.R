
genotypevar <- function(variable="FT.q",
                        applyfilter=T,
                        site=c("madrid","tuebingen"),
                        water=c("h","l"),
                        indpop=c("i","p"),
                        random="id",
                        relative=T,
                        method="lmer",
                        thedata=field
                        )
{

  # build the lm formula
  if(relative==T){   fix=paste("relative(",variable,") ~ 1")  }
  else{  fix=paste(variable," ~ 1")   }

  rand= paste("+ (1|",random,")", collapse=" ")

  myformu=paste(fix,rand)

  # print (myformu)

  # filter data if necessary
  if(applyfilter==T){
    mydata<-thedata[which(thedata$site %in% site & thedata$water %in% water & thedata$indpop %in% indpop),]
  } else{ mydata<-thedata }


  mydata.rm<-na.omit(mydata[,c(variable,"site","water","indpop",random)])


  # run lmm
  study <- lmer(data=mydata.rm,
                myformu)

  # get the variance of random components
  randomvariance(study,var=random)

}

randomvariance<-function(lmod,var='id'){ ############## <<<<<<<<<<<<<<<<<<<<<<<<<<< EDIT THIS TO ALLOT MCMCGLMM TO GET THE VARIANCE

  if(class(lmod)=='lmerMod'){
  varcovar=as.data.frame(VarCorr(lmod))
  res<-dplyr::filter(varcovar, grp %in% var) [,"vcov"] / sum(varcovar[,"vcov"])
  res=as.matrix(res)
  }
  if(class(lmod)=='MCMCglmm'){
  res=h2MCMC(lmod,randomname=var)
  }

  return(res)
}

