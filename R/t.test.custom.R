#' t test for slope agains an arbitrary number
#'
#' @param reg
#' @param coefnum
#' @param val
#'
#' @return
#' @export
#'
#' @examples
t.test.custom <- function(reg, coefnum=2, val){
  co <- coef(summary(reg))
  tstat <- (co[coefnum,1]-val)/co[coefnum,2]
  2 * pt(abs(tstat), reg$df.residual, lower.tail = FALSE)
}
