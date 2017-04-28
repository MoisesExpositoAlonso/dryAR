LDphysical <- function(pos){
x=1:length(pos)
y=1:length(pos)
long<- expand.grid(x,y)
colnames(long)=c("x","y")

d=apply(long,1,function(x){
    d <- abs( pos[x[1]] -  pos[x[2]] )
  return( d )
})
long$d <- d
head(long)
d=tidyr::spread(long,value = d, key = x,y) [,-1]
r=1/d
r[r==Inf]=2
return(as.matrix(r))
}
