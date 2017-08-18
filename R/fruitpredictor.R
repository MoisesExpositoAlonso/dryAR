

fruitpredictor<-function(){
data("fruitcounted")

mod.sk=lm(fruitcounted$nfruits ~ fruitcounted$Harv.sk +0 )
mod.area<-lm(fruitcounted$nfruits ~ fruitcounted$Harv.area +0)
mod.bp<-lm(fruitcounted$nfruits ~ fruitcounted$Harv.bp +0)
mod.ep<-lm(fruitcounted$nfruits ~ fruitcounted$Harv.ep +0)
mod.all<-lm(data=fruitcounted, nfruits ~ Harv.sk + Harv.area+ Harv.bp +Harv.ep +0)

return(list(sk=mod.sk, area=mod.area,bp=mod.bp, ep=mod.ep,all=mod.all))

}


usefruitpredictor<-function(dat,from='all'){
     if(from=='sk') predict(object = fruitpredictor()$sk,newdata = dat )
else if(from=='area') predict(object = fruitpredictor()$area,newdata = dat )
else if(from=='bp') predict(object = fruitpredictor()$bp,newdata = dat )
else if(from=='ep') predict(object = fruitpredictor()$ep,newdata = dat )
else if(from=='all') predict(object = fruitpredictor()$all,newdata = dat )


}
