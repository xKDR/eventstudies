## Function to retain NA positions
ARinnovations <- function(x) {
  stopifnot(NCOL(x) == 1)
  dt <- NULL
  if (any(c("xts","zoo") %in% class(x))) {
    dt <- index(x)
    x <- as.numeric(x)
  }
  non.na.locations <- !is.na(x)
  x <- x[non.na.locations]
  m <- ar(x)
  e <- m$resid
  # Relocate with the old NA structure
  result <- rep(NA, length(non.na.locations))
  result[non.na.locations] <- e
  if (!is.null(dt)) {result <- zoo(result, order.by=dt)}
  list(result=result, m=m)
}

## The workhorse called by makeX to return a nice matrix of RHS
## variables to be used in an analysis. 
do.one.piece <- function(market.returns, others, switch.to.innov, market.returns.purge, nlags, verbose=FALSE) {
  thedates <- index(market.returns)
  if (verbose) {
    cat("   Doing work for period from ",
        as.character(head(thedates,1)), " to ",
        as.character(tail(thedates,1)), "\n")
  }
  otherlags <- NULL
  for (i in 1:NCOL(others)) {
    if (switch.to.innov[i]) {
      a <- ARinnovations(others[,i])
      innovated <- a$result
      if (verbose) {
        cat("   AR model for ", colnames(others, do.NULL=FALSE)[i], "\n")
        print(a$m)
      }
      otherlags <- c(otherlags, a$m$order)
    } else {
      innovated <- others[,i]
      otherlags <- c(otherlags, 0)
    }
    if (i > 1) {
      innov <- cbind(innov, innovated)
    } else {
      innov <- innovated
    }
  }
  if (NCOL(innov) > 1) {colnames(innov) <- colnames(others)}
  market.returns.purged <- market.returns
  if (market.returns.purge) {
    firstpass <- TRUE
    for (i in 1:NCOL(innov)) {
      for (j in 0:nlags) {
        if (firstpass) {
          z <- lag(innov[,i],-j)
          labels <- paste(colnames(innov,do.NULL=FALSE)[i],j,sep=".")
          firstpass <- FALSE
        } else {
          z <- cbind(z, lag(innov[,i], -j))
          labels <- c(labels, paste(colnames(innov,do.NULL=FALSE)[i],j,sep="."))
        }
      }
    }
    if (NCOL(z) > 1) {colnames(z) <- labels}
    m <- lm(market.returns ~ ., as.data.frame(cbind(market.returns, z)))
    if (verbose) {
      cat("   Model explaining market.returns:\n")
      print(summary(m))
    }
    how.many.NAs <- nlags + max(otherlags)
    market.returns.purged <- zoo(c(rep(NA,how.many.NAs),m$residuals),
                      order.by=thedates)
  }
                                        #    if (verbose) {cat("   Finished do.one.piece()\n")}
  list(market.returns.purged=market.returns.purged, innov=innov)
}                              

# A function that calls do.one.piece, and works through several
# different periods to provide the right RHS matrix. 
makeX <- function(market.returns, others,
                  switch.to.innov=rep(TRUE, NCOL(others)),
                  market.returns.purge=TRUE,
                  nlags=5,
                  dates=NULL,
                  verbose=FALSE) {
  if (verbose) {cat("0. Checking args\n")}
  stopifnot(all.equal(index(market.returns), index(others)),
            length(switch.to.innov)==NCOL(others))
  if (!is.null(dates)) {
    stopifnot(class(dates) == "Date")
  }
  if (verbose) {cat("1. Checking dates.\n")}
  if (is.null(dates)) {
    dates <- c(start(market.returns),end(market.returns))
  }
  if(head(dates,1)!=head(index(market.returns),1)){
    stop("Start date provided and the start date of the dataset do not match \n")
  }
  if(tail(dates,1)!=tail(index(market.returns),1)){
    stop("End date provided and the end date of the dataset do not match \n")
  }
  if (verbose) {cat("2. Run through all the pieces --\n")}
  for (i in 1:(length(dates)-1)) {
    t1 <- dates[i]
    t2 <- dates[i+1]
    if (i != (length(dates)-1)) {t2 <- t2 -1}
    if (verbose) {
      cat("   Focusing down from date = ", as.character(t1), " to ", as.character(t2), "\n")
    }
    tmp.market.returns <- window(market.returns, start=t1, end=t2)
    tmp.others <- window(others, start=t1, end=t2)
    a <- do.one.piece(tmp.market.returns, tmp.others, switch.to.innov, market.returns.purge, nlags, verbose)
    if (i > 1) {
      res.market.returns <- c(res.market.returns, a$market.returns.purged)
      res.innov <- rbind(res.innov, a$innov)
    } else {
      res.market.returns <- a$market.returns.purged
      res.innov <- a$innov
    }
  }
  if (verbose) {cat("2. Make a clean X and send it back --\n")}
  X <- cbind(res.market.returns, res.innov)
  if (NCOL(res.innov) == 1) {colnames(X) <- c("market.returns","z")}
  else {colnames(X) <- c("market.returns", colnames(res.innov))}
  X
}

## One regressor, one period AMM estimation
lmAMM <- function(firm.returns, X, nlags=NULL, verbose=FALSE) {
  do.ols <- function(nlags) {
    tmp <- cbind(firm.returns, X[,1]) # Assume 1st is stock index, and no lags are required there.
    labels <- c("firm.returns","market.returns")
    if (NCOL(X) > 1) {
      for (i in 2:NCOL(X)) {
        for (j in 0:nlags) {
          tmp <- cbind(tmp, lag(X[,i], -j))
          labels <- c(labels, paste(colnames(X)[i], j, sep="."))
        }
      }
    }
    tmp <- na.omit(tmp)
    if (nrow(tmp) < 30) {             # refuse to do the work.
      return(NULL)                    # returns out of do.ols() only
    }

    colnames(tmp) <- labels          # So the OLS results will look nice
    lm(firm.returns ~ ., data=as.data.frame(tmp))
  }

  if (is.null(nlags)) {
    if(verbose) {cat("Trying to find the best lag structure...\n")}
    bestlag <- 0
    bestm <- NULL
    bestAIC <- Inf
    for (trylag in 0:min(10,log10(length(firm.returns)))) {
      thism <- do.ols(trylag)
      if (is.null(m)) {return(NULL)}
      thisAIC <- AIC(thism, k=log(length(thism$fitted.values)))
      if (verbose) {cat(trylag, " lags, SBC = ", thisAIC, "\n")}
      if (thisAIC < bestAIC) {
        bestlag <- trylag
        bestAIC <- thisAIC
        bestm <- thism
      }
    }
    nlags <- bestlag
    m <- bestm
  } else {
    m <- do.ols(nlags)
    if (is.null(m)) {return(NULL)}
  }
  # In either event, you endup holding an "m" here.
  if (verbose) {cat("\n\nThe OLS:\n"); print(summary(m))}

  # Compute a series of exposure measures, and their standard errors.
  beta <- m$coefficients
  Sigma <- vcovHAC(m)
  # First the market.returns
  exposures <- beta[2]                  # no lags for market.returns
  s.exposures <- sqrt(Sigma[2,2])
  # From here on, there's a block of 1+nlags coeffs for each
  # of the non-market.returns regressors.
  if (NCOL(X) > 1) {
    for (i in 2:NCOL(X)) {
      n.block1 <- 2 + ((i-2)*(1+nlags)) # Just 2 for the 1st case.
      n.block2 <- length(beta) - n.block1 - (1 + nlags)
      w <- c(rep(0, n.block1), rep(1, 1+nlags), rep(0, n.block2))
      exposures <- c(exposures, w %*% beta)
      s.exposures <- c(s.exposures, sqrt(w %*% Sigma %*% w))
    }
  }
  results <- m
  names(exposures) <- names(s.exposures) <- colnames(X)
  results$exposures <- exposures
  results$s.exposures <- s.exposures
  results$nlags <- nlags
  class(results) <- "amm"
  return(results)
}

## One regressor, with sub periods. 
subperiod.lmAMM <- function(firm.returns,X,nlags=1,verbose=FALSE,dates=NULL,residual=TRUE){
  ## Creating empty frames
  if(is.null(dates)){
    dates.no <- c(start(firm.returns),end(firm.returns))
  } else{
    dates.no <- dates
  }
  exposures <- data.frame(matrix(NA,ncol=ncol(X),nrow=(length(dates.no)-1)))
  colnames(exposures) <- colnames(X)
  sds <- exposures
  periodnames <- NULL
  
  ## Getting firm exposure, amm residuals
  if(is.null(dates)){
   res <- lmAMM(firm.returns,X,verbose=verbose,nlags=nlags)
   if(is.null(res)!=TRUE){
     exposures <- res$exposure
     sds <- res$s.exposure
     m.residuals <- xts(res$residuals,as.Date(attr(res$residuals,"names")))
     if(residual==TRUE){
       m.residuals <- xts(res$residuals,as.Date(attr(res$residuals,"names")))
     }
     rval <- list(exposures=exposures,sds=sds,residuals=m.residuals)
   } else {
     rval <- NULL
   }
 }else{
   tmp <- window(firm.returns,start=dates[1],end=dates[1+1])
   rhs <- window(X,start=dates[1],end=dates[1+1])
   res <- lmAMM(firm.returns=tmp,
                        X=rhs,
                        verbose=verbose,
                        nlags=nlags)
   exposures[1,] <- res$exposure
   periodnames <- c(periodnames,paste(dates[1],dates[1+1],sep=" TO "))
   sds[1,] <- res$s.exposure
   m.residuals <- xts(res$residuals,as.Date(attr(res$residuals,"names")))
   colnames(m.residuals) <- paste(dates[1],"to",dates[1+1],sep=".")
   for(i in 2:(length(dates)-1)){
     tmp <- window(firm.returns,start=dates[i],end=dates[i+1])
     rhs <- window(X,start=dates[i],end=dates[i+1])
     res <- lmAMM(firm.returns=tmp,
                          X=rhs,
                          verbose=verbose,
                          nlags=nlags)
     exposures[i,] <- res$exposure
     periodnames <- c(periodnames,paste(dates[i],dates[i+1],sep=" TO "))
     sds[i,] <- res$s.exposure
     period.resid <- xts(res$residuals,as.Date(attr(res$residuals,"names")))
     colnames(period.resid) <- paste(dates[i],"to",dates[i+1],sep=".")
     m.residuals <- merge(m.residuals, period.resid, all=TRUE)
   }
   rownames(exposures) <- rownames(sds) <- periodnames
   rval <- list(exposures=exposures,sds=sds,residuals=m.residuals)
 } 
  return(rval)
}

## Many regressors, many periods, one matrix of RHS
manyfirmssubperiod.lmAMM <- function(firm.returns,X,
                          lags,dates=NULL, periodnames=NULL,verbose=FALSE){
  if(is.null(dates)){
    dates=c(start(X),end(X))
    periodnames="Full"
  }
  nperiods <- length(periodnames)
  if(length(dates) != (nperiods+1)){
    cat("Mistake in length of dates versus length of periods.\n")
    return(NULL)
  }
  nfirms <- ncol(firm.returns)

  # Let's get "exposure' and 'sds'. Setting up structures:-

  exposures <- matrix(NA,nrow=nfirms,ncol=nperiods*ncol(X))
  exposures <- as.data.frame(exposures)
  rownames(exposures) <- colnames(firm.returns)
  tmp <- NULL 
  for(i in 1:length(periodnames)){
    for(j in 1:NCOL(X)){
      tmp <-  c(tmp, paste(colnames(X)[j],
                           periodnames[i],sep="."))
    }
  }
  colnames(exposures) <- tmp
  sds <- exposures
  colnames(sds) <- paste("sd",colnames(exposures),sep=".")

  # Setup a list structure for an OLS that failed
  empty <- list(exposures=rep(NA,ncol(X)),
                s.exposures=rep(NA,ncol(X)))
  
  for(i in 1:NCOL(firm.returns)){
    cat("AMM estimation for",colnames(firm.returns)[i],"\n")
    if (verbose) {cat ("AMM estimation for", colnames(firm.returns)[i], "\n")}
    stock.return <- firm.returns[,i]
    dataset <- cbind(stock.return, X)   # This is the full time-series
    this.exp <- this.sds <- NULL
    for(j in 1:nperiods){              # now we chop it up 
      t1 <- dates[j]
      t2 <- dates[j+1]
      this <- window(dataset,start=t1, end=t2)
      fe <- lmAMM(this[,1],this[,-1],nlags=lags,verbose)
      if(is.null(fe)) {fe <- empty}
      this.exp <- c(this.exp, fe$exposures)
      this.sds <- c(this.sds, fe$s.exposures)
    }
    exposures[colnames(firm.returns)[i],] <- this.exp
    sds[colnames(firm.returns)[i],] <- this.sds
  }
  list(exposures=exposures, sds=sds, sig=exposures/sds)
}

############################################
## Summary, print and plot functions for AMM
############################################
summary.amm <- function(object, ...) {
  cat("\n", "Summary statistics of exposure: \n")
  sstats <- cbind(object$exposure, object$s.exposure,
                  object$exposure/object$s.exposure)
  colnames(sstats) <- c("Exposure", "Std.Err", "t statistic")  
  rownames(sstats) <- names(object$exposures)
  print(sstats)
  cat("\n","Linear model AMM  results: ","\n");
  class(object) <- "lm";
  print.default(summary.default(object))
}

print.amm <- function(x, ...){
  cat("\n")
  print(x$call)
  cat("\n","Coefficients:","\n")
  print(x$coef)
  cat("\n","Exposures:","\n")
  print.default(x$exposures)
}

plot.amm <- function(x, ...){
  tmp.x <- zoo(as.numeric(resid(x)), as.Date(names(resid(x))))
  tmp.f <- zoo(x$model$firm.returns, index(tmp.x))
  tmp <- merge(tmp.x,tmp.f)
  colnames(tmp) <- c("amm.residuals","firm.returns")
  plot(tmp, screen=1, lty=1:2, lwd=2, col=c("indian red", "navy blue"),ylab="",
       xlab="")
  legend("topleft",legend=c("AMM residual","Firm returns"),lty=1:2, lwd=2,
         col=c("indian red", "navy blue"), bty='n')
}
