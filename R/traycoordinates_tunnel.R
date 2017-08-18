
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
