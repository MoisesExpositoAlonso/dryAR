
makerows <- function(){
f.r <-rep(sort(rep(c(1:44),8)) )
f.r.p <- cbind(f.r,f.r,f.r,f.r,f.r)
f.r.p <- cbind(f.r.p,f.r.p,f.r.p,f.r.p,f.r.p,f.r.p,f.r.p,f.r.p)
}
makecols <- function(){
f.c<-sort(rep(1:8,5))
f.c.p <- eval(parse(text = paste("rbind(" ,paste(rep("f.c",352),collapse =  ",") ,")")  ))
}
