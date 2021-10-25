#' newfunctionfromabove
#'
#' @param n vector, sample size
#' @param iter vector, iterations
#' @param a vector, starting value
#' @param b vector, end value

#' @importFrom graphics hist
#' @importFrom stats runif
#' @return a histogram of the sums of the uniform distributions
#' @export
#'
#' @examples
#' newfunctionfromabove(10,10,0,5)
newfunctionfromabove=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm)
  sm
}
