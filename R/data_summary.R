data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}
data_summary_median <- function(x) {
   m <- quantile(x,probs = 0.5)%>% as.numeric()
   ymin <- quantile(x,probs = 0.75) %>% as.numeric()
   ymax <-quantile(x,probs = 0.25)%>% as.numeric()
   return(c(y=m,ymin=ymin,ymax=ymax))
}
