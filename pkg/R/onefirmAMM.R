#######################
# AMM for one firm
#######################
source('AMM.R')
onefirmAMM <- function(rj,X,nlags=NA,verbose=FALSE,dates=NULL){
  exposures <- data.frame(matrix(NA,ncol=ncol(X),nrow=(length(dates)-1)))
  colnames(exposures) <- colnames(X)
  sds <- exposures
  periodnames <- NULL
  
  if(is.null(dates)){
   res <- firmExposures(rj,X=rhs,verbose=verbose,nlags=nlags)
   exposures <- res$exposure
   sds <- res$s.exposure
 }else{
   for(i in 1:(length(dates)-1)){
     tmp <- window(rj,start=dates[i],end=dates[i+1])
     rhs <- window(X,start=dates[i],end=dates[i+1])
     res <- firmExposures(rj=tmp,
                          X=rhs,
                          verbose=verbose,
                          nlags=nlags)
     exposures[i,] <- res$exposure
     periodnames <- c(periodnames,paste(dates[i],dates[i+1],sep=" TO "))
     sds[i,] <- res$s.exposure
   }
   rownames(exposures) <- rownames(sds) <- periodnames
 }
  rval <- list(exposures=exposures,sds=sds)
  return(rval)
}
