#' Title My Boot 2
#'
#' @param iter iterations as a vector
#' @param x sample values as a vector
#' @param fun function that will be used to find interval
#' @param alpha vector
#' @param cx vector
#' @param ... any other
#' @importFrom graphics abline segments
#' @importFrom stats quantile
#'
#' @return a list of xstat and a histogram with the
#' bootstrap interval and point estimate
#' @export
#'
#' @examples
#' myboot2(iter=10000,x=c(1,2),fun="mean",alpha=0.01)
myboot2<-function(iter=10000,x=NA,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics",
                       "\n","alpha=",alpha," iter=",iter,sep=""),...)
    mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  return(list(ci=ci,fun=fun,x=x,xstat=xstat))
}
