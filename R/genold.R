genold<-function(path){
  if(!is.character(path))
    stop("path must be a character")
  .Call("genold",path)
}
