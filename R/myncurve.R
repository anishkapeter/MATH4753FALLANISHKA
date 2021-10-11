#' Title Myncurve
#'
#' @param mu vector of means of normal curve
#' @param sigma vector of standard deviations of normal curve
#' @param a vector of observations
#'
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#' @return a graph
#' @export
#'
#' @examples
#' myncurve(4,3,2)
myncurve = function(mu, sigma,a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma),xlim= c(mu-3*sigma,mu+ 3*sigma))
  xcurve=seq(mu-10*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-10*sigma,xcurve,a),c(0,ycurve,0),col="red")
  prob=pnorm(a,mean=mu,sd=sigma)
  list(paste("Area =", round(prob,4),sep=" "))
}
