removetail<-function(x,positions=4){
  as.numeric(substrRight( x, lastpos = positions,giveright = F))

}

getsigmoid<-function(a=100000,b=0.5,c=25){
a=as.numeric(a)
b=as.numeric(b)
c=as.numeric(c)

x=c(1:40)
a/(1 + exp(-b * (x-c)))

}

fitspline<-function(y,x){
fits<-spline(y~x)
which(fits$y >1000)[1]
}


fitsigmoid<-function(y,x,parameter=NULL){
# y=y+10 # it is +1 because 0 give errors of convergence
mydata=data.frame(y,x) %>% arrange(x)
# maxpoint=which(mydata$y == max(mydata$y)) # before I used directly the maximum point
maxpoint = which(diff(y) == min(diff(y)))-1
# instead of looking for the maximum point, it looks for the maximum reduction, which must be when it already has reached the max.
mydata=mydata[1:maxpoint,]


tryCatch({

      fitmodel <- nls(data = mydata,(y)~a/(1 + exp(-b * (x-c))), start=list(a=max(y),b=0.25,c=25))
      res=summary(fitmodel)$coefficients

      toreport=list(a=paste0(res["a",'Estimate'], starpvalue(res["a",4],samelength=T)),
                    b=paste0(res["b",'Estimate'], starpvalue(res["b",4],samelength=T)),
                    c=paste0(res["c",'Estimate'], starpvalue(res["c",4],samelength=T))
                    )
      if(is.null(parameter)){return(toreport)}
      else if(parameter%in%c('a','b','c')){ return(as.character(toreport[parameter])) }
      else{stop('Did not provide a valid name for parameter flag!')}


    },

    warning = function(war) {return('NA')}, # it is stupid, it produces error if I just put NA, but not w 'NA'
    error = function(e) {return('NA')}

    )

}

fitlinear<-function(y,x,parameter=NULL){

mydata=data.frame(y,x) %>% arrange(x)
maxpoint=which(mydata$y == max(mydata$y))
mydata=mydata[1:maxpoint,]

tryCatch({

      mod=lm(y ~ x,data=mydata)

      xintercept<-(0-mod$coefficients[1]) /mod$coefficients[2]

      toreport=ifelse(xintercept >40 | xintercept< -10, 'NA', xintercept)

      if(is.null(parameter)){return(toreport)}
      else if(parameter=='significance'){
        return(
        paste0(toreport, "_",
        starpvalue(summary(mod)$coefficients[2,4],samelength=T),
        round(summary(mod)$r.squared,digits = 1) )
        )
      }else{
          stop('Did not provide a valid name for parameter flag! either NULL or significance')
        }


      return(toreport)
    },
    warning = function(war) {return('NA')}, # it is stupid, it produces error if I just put NA, but not w 'NA'
    error = function(e) {return('NA')}
    )
}

firstgreen<-function(y,x){
  mydata=data.frame(x,y) %>% arrange(x)
  # theday<-which(mydata$y > 800) # THRESHOLD ARBITRARILY DECIDED, BUT INFORMED BECAUSE USUALLY WHEN CLEAR COTILEDONS ARE THERE THERE ARE >1000 PIXELS
  theday<-which(mydata$y > 1000) # THRESHOLD ARBITRARILY DECIDED, BUT INFORMED BECAUSE USUALLY WHEN CLEAR COTILEDONS ARE THERE THERE ARE >1000 PIXELS

  if(length(theday)==0){
    return(NA)
    }else{
    return(mydata$x [theday[1]])
  }
}







######################################################################

# fitgermination<-function(y,x){
#   mydata=data.frame(x,y) %>% filter(y!=0)
#   if(nrow(mydata)<5){
#     return(NA)
#   }
#   else{
#   mod=lm(y ~ x,data=mydata)
#   mod=lm(y ~ x,data=mydata)
#
#   xintercept<-(0-mod$coefficients[1]) /mod$coefficients[2]
#
#       if(xintercept > 40){xintercept<-NA}
#     else if(xintercept <0){
#         if(xintercept<10){ xintercept<-0
#         }else{xintercept<-NA}
#           }
#   return(xintercept)
#   }
# }
#
# r2fitgermination<-function(y,x){
#   mydata=data.frame(x,y) %>% filter(y!=0)
#   if(nrow(mydata)<5){
#     return(NA)
#   }
#   else{
#   mod=lm(y ~ x,data=mydata)
#   return(summary(mod)$r.squared)
#   }
# }
#
# pfitgermination<-function(y,x){
#   mydata=data.frame(x,y) %>% filter(y!=0)
#   if(nrow(mydata)<5){
#     return(NA)
#   }
#   else{
#   mod=lm(y ~ x,data=mydata)
#   return(summary(mod)$coefficients[2,4])
#   }
# }
#
# firstgreen<-function(y,x){
#   mydata=data.frame(x,y) %>% arrange(x)
#   # theday<-which(mydata$y > 800) # THRESHOLD ARBITRARILY DECIDED, BUT INFORMED BECAUSE USUALLY WHEN CLEAR COTILEDONS ARE THERE THERE ARE >1000 PIXELS
#   theday<-which(mydata$y > 1000) # THRESHOLD ARBITRARILY DECIDED, BUT INFORMED BECAUSE USUALLY WHEN CLEAR COTILEDONS ARE THERE THERE ARE >1000 PIXELS
#
#   if(length(theday)==0){
#     return(NA)
#     }else{
#     return(mydata$x [theday[1]])
#   }
# }
#

