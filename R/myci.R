#' Title myci
#'
#' @param x a vector of data for the sample
#'
#' @return 2 numbers that give a 95% ci
#' @export
#'
#' @examples
#' myci(2,3,4)
myci=function(x){
  n=length(x)
  t=qt(1-0.05/2,n-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci
}
