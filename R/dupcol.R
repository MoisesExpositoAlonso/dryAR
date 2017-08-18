dupcol=function(data1,data2){
dupcol<-c(
  colnames(data1) ,
  colnames(data2)
)
dupcol[duplicated(dupcol)]
}
