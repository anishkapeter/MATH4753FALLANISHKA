#' Title mymaxlikg
#'
#' @param lfun a function
#' @param theta a vector
#' @importFrom graphics axis
#'
#' @return a graph that estimates the maximum likelihood estimate
#' @export
#'
#' @examples
#' \dontrun{mymaxlikg(lfun="logbin",theta=seq(0,1,length=10000))}
mymaxlikg=function(lfun="logbin2",theta) {
  nth=length(theta)
  thmat=matrix(theta,nrow=nth,ncol=1,byrow=TRUE)
  z=apply(thmat,1,lfun)
  zmax=max(which(z==max(z)))
  plot(theta,exp(z),type="l")
  abline(v=theta[zmax],col="Blue")
  axis(3,theta[zmax],round(theta[zmax],4))
  theta[zmax]
}
