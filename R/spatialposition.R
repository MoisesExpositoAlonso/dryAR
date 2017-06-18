spatialposition<-function(){

# x<-c()
#
# 44*

}



makerows <- function(){
f.r <-rep(sort(rep(c(1:44),8)) )
f.r.p <- cbind(f.r,f.r,f.r,f.r,f.r)
f.r.p <- cbind(f.r.p,f.r.p,f.r.p,f.r.p,f.r.p,f.r.p,f.r.p,f.r.p)
}
makecols <- function(){
f.c<-sort(rep(1:8,5))
f.c.p <- eval(parse(text = paste("rbind(" ,paste(rep("f.c",352),collapse =  ",") ,")")  ))
}



traycoordinates_tunnel <- function(){
traycoord <- traycoordinates()
traycoordm <-t(matrix(traycoord,nrow=5,ncol=8,byrow = T) )
traycoordm<-traycoordm[, rev(c(1:5))]
# put the coordinates of the whole file
tc <-traycoordm
tc2 <- cbind(tc,tc,tc,tc,tc,tc,tc,tc)
tc3 <- eval(parse(text = paste("rbind(", paste(rep("tc2",44),collapse=","),")")))

return(tc3)
}


traycoordinates<-function(){
# make coordinate tray index
traycoord <-(c("x",'a2', 'a3', 'a4', 'a5', 'a6', 'a7',"x", 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'd1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8',"x", 'e2', 'e3', 'e4', 'e5', 'e6', 'e7',"x") )
return(traycoord)
}
