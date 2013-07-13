
########################
# Many firms AMM
########################
source('AMM.R')
manyfirmsAMM <-function(regressand,regressors,
                        lags,dates=NULL, periodnames=NULL,verbose=FALSE){
  if(is.null(dates)){
    dates=c(start(regressors),end(regressors))
    periodnames="Full"
  }
  nperiods <- length(periodnames)
  if(length(dates) != (nperiods+1)){
    cat("Mistake in length of dates versus length of periods.\n")
    return(NULL)
  }
  nfirms <- ncol(regressand)

  # Let's get "exposure' and 'sds'. Setting up structures:-

  exposures <- matrix(NA,nrow=nfirms,ncol=nperiods*ncol(regressors))
  rownames(exposures) <- colnames(regressand)
  tmp <- NULL
  for(i in 1:length(periodnames)){
    for(j in 1:ncol(regressors)){
      tmp <-  c(tmp, paste(colnames(regressors)[j],
                           periodnames[i],sep="."))
    }
  }
  colnames(exposures) <- tmp
  sds <- exposures
  colnames(sds) <- paste("sd",colnames(exposures),sep=".")

  # Setup a list structure for an OLS that failed
  empty <- list(exposures=rep(NA,ncol(regressors)),
                s.exposures=rep(NA,ncol(regressors)))

  for(i in 1:ncol(regressand)){
	cat("Doing",colnames(regressand)[i])
    if (verbose) {cat ("Doing", colnames(regressand)[i])}
    rj <- regressand[,i]
    dataset <- cbind(rj, regressors)   # This is the full time-series
    this.exp <- this.sds <- NULL
    for(j in 1:nperiods){              # now we chop it up 
      t1 <- dates[j]
      t2 <- dates[j+1]
     # if (j != nperiods) {t2 <- t2-1} # I don't know why I wrote this now :-( But this created problems.
      this <- window(dataset,start=t1, end=t2)
      fe <- firmExposures(this[,1],this[,-1],nlags=lags,verbose)
      if(is.null(fe)) {fe <- empty}
      this.exp <- c(this.exp, fe$exposures)
      this.sds <- c(this.sds, fe$s.exposures)
    }
    exposures[i,] <- this.exp
    sds[i,] <- this.sds
  }
  list(exposures=exposures, sds=sds, sig=exposures/sds)
}
