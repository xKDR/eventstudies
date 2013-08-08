##########################
# Generalised AMM function
##########################

AMM <- function(amm.type = NULL, ...) {

  ## List of models currently supported
  modelsList <- c("all","residual","firmExposures")

  if (is.null(amm.type) || length(amm.type) != 1) {
    stop("Argument amm.type not provided or incorrect")
  }
  if (match(amm.type, modelsList, nomatch = -1) == -1) {
    stop("Unknown model provided")
  }

                                        # NULLify all the values before use
  firm.returns <- NULL
  market.returns <- NULL
  market.returns.purge <- NULL
  nlags <- NULL
  others <- NULL
  switch.to.innov <- NULL
  verbose <- NULL
  dates <- NULL

                                     # parse the input arguments for the model
  modelArgs <- list(...)
                                        # assign values
  for (i in 1:length(modelArgs)) {
    eval(parse(text = paste(names(modelArgs)[i], "<-", "modelArgs[[i]]")))
  }

                                      # Checking required arguments
  if (match("market.returns", names(modelArgs), nomatch = -1) == -1) {
    stop("Input market.returns (stock market index) is missing")
  }
  if (match("others", names(modelArgs), nomatch = -1) == -1) {
    stop("Input 'others' (time series of other regressor or interest) is missing")
  }
  if (match("market.returns.purge", names(modelArgs), nomatch = -1) == -1) {
    stop("Input market.returns.purge is missing")
  }
  if (match("switch.to.innov", names(modelArgs), nomatch = -1) == -1) {
    stop("Input switch.to.innov is missing")
  }
  
                                        # Checking remaining arguments
  if (match("nlags", names(modelArgs), nomatch = -1) == -1) {
    nlags <- 1
  }
  if (match("verbose", names(modelArgs), nomatch = -1) == -1) {
    verbose <- FALSE
  }
  if (match("dates", names(modelArgs), nomatch = -1) == -1) {
    dates <- NULL
  }

  ## Assign values
  
  ##----
  ## AMM
  ##----
  if(amm.type == "residual") {
    ## One firm
    if(NCOL(firm.returns)==1){
                                        # Checking required arguments
      if (match("firm.returns", names(modelArgs), nomatch = -1) == -1) {
        stop("Input firm.returns (firm data) is missing")
      }
      
      X <- makeX(market.returns, others, switch.to.innov,
                 market.returns.purge, nlags, dates, verbose)
      result <- onefirmAMM(firm.returns, X, nlags, verbose, dates)
      result <- result$residuals
    }
    
    ## Many firms
    if(NCOL(firm.returns)>1){
                                           # Checking required arguments
    if (match("firm.returns", names(modelArgs), nomatch = -1) == -1) {
      stop("Input firm.returns (firm data) is missing")
    }
    if(NCOL(firm.returns)<2){
      stop("Less than two firms in inputData")
    }
    
    X <- makeX(market.returns, others, switch.to.innov,
               market.returns.purge, nlags, dates, verbose)
    result <- xts()
    for(i in 1:NCOL(firm.returns)){
      tmp <- onefirmAMM(firm.returns[,i], X, nlags, verbose, dates)
      result <- merge(result,tmp$residuals)
    }
    colnames(result) <- colnames(firm.returns)
    }
  }

  ##----
  ## All
  ##----
  if(amm.type == "all") {
    ## One firm
    if(NCOL(firm.returns)==1){
                                        # Checking required arguments
      if (match("firm.returns", names(modelArgs), nomatch = -1) == -1) {
        stop("Input firm.returns (firm data) is missing")
      }
      
      X <- makeX(market.returns, others, switch.to.innov,
                 market.returns.purge, nlags, dates, verbose)
      result <- onefirmAMM(firm.returns, X, nlags, verbose, dates)
    }
    
    ## Many firms
    if(NCOL(firm.returns)>1){
                                           # Checking required arguments
    if (match("firm.returns", names(modelArgs), nomatch = -1) == -1) {
      stop("Input firm.returns (firm data) is missing")
    }
    if(NCOL(firm.returns)<2){
      stop("Less than two firms in inputData")
    }
    
    X <- makeX(market.returns, others, switch.to.innov,
               market.returns.purge, nlags, dates, verbose)
    result <- list()
    for(i in 1:NCOL(firm.returns)){
      tmp <- onefirmAMM(firm.returns[,i], X, nlags, verbose, dates)
      result[[i]] <- tmp
    }
    names(result) <- colnames(firm.returns)
    }
  }

  ##---------------
  ## Firm exposures
  ##---------------
  if (amm.type=="firmExposures") {
                                        # Checking required arguments
    if (match("firm.returns", names(modelArgs), nomatch = -1) == -1) {
      stop("Input firm.returns (firm data) is missing")
    }
    
    X <- makeX(market.returns, others, switch.to.innov,
               market.returns.purge, nlags, dates, verbose)

    result <- firmExposures(firm.returns, X, nlags, verbose)
  }

  return(result)
}

#######################
# AMM for one firm
#######################
onefirmAMM <- function(firm.returns,X,nlags=1,verbose=FALSE,dates=NULL,residual=TRUE){
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
   res <- firmExposures(firm.returns,X,verbose=verbose,nlags=nlags)
   exposures <- res$exposure
   sds <- res$s.exposure
   m.residuals <- xts(res$residuals,as.Date(attr(res$residuals,"names")))
   if(residual==TRUE){
     m.residuals <- xts(res$residuals,as.Date(attr(res$residuals,"names")))
   }
 }else{
   tmp <- window(firm.returns,start=dates[1],end=dates[1+1])
   rhs <- window(X,start=dates[1],end=dates[1+1])
   res <- firmExposures(firm.returns=tmp,
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
     res <- firmExposures(firm.returns=tmp,
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
 }
  rval <- list(exposures=exposures,sds=sds,residuals=m.residuals)
  return(rval)
}

########################
# Many firms AMM
########################
manyfirmsAMM <-
function(regressand,regressors,
                          lags,dates=NULL, periodnames=NULL,verbose=FALSE){
  require("doMC")
  registerDoMC()
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
    firm.returns <- regressand[,i]
    dataset <- cbind(firm.returns, regressors)   # This is the full time-series
    this.exp <- this.sds <- NULL
    for(j in 1:nperiods){              # now we chop it up 
      t1 <- dates[j]
      t2 <- dates[j+1]
      this <- window(dataset,start=t1, end=t2)
      fe <- firmExposures(this[,1],this[,-1],nlags=lags,verbose)
      if(is.null(fe)) {fe <- empty}
      this.exp <- c(this.exp, fe$exposures)
      this.sds <- c(this.sds, fe$s.exposures)
    }
    exposures[colnames(regressand)[i],] <- this.exp
    sds[colnames(regressand)[i],] <- this.sds
  }
  list(exposures=exposures, sds=sds, sig=exposures/sds)
}

###############################################
# Estimating one firm's exposure in one period.
###############################################
firmExposures <- function(firm.returns, X, nlags=NA, verbose=FALSE) {
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

  if (is.na(nlags)) {
    if (verbose) {cat("Trying to find the best lag structure...\n")}
    bestlag <- 0
    bestm <- NULL
    bestAIC <- Inf
    for (trylag in 0:min(10,log10(length(firm.returns)))) {
      thism <- do.ols(trylag)
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
  residuals <- m$resid
  names(exposures) <- names(s.exposures) <- colnames(X)
  results <- list(exposures=exposures,residuals=residuals,
                  s.exposures=s.exposures, nlags=nlags,
                  lm.res=m)
  class(results) <- "amm"
  results
}

###########################
# Maintaing NAs in AR model
###########################
ARinnovations <- function(x) {
  stopifnot(NCOL(x) == 1)
  dt <- NULL
  if (class(x) == "zoo") {
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

# ---------------------------------------------------------------------------
# The workhorse called by makeX to return a nice matrix of RHS
# variables to be used in an analysis. 
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

#############################################################################
# Exposure is a vector of exposures
# var.exposures is a vector of the s.error of each exposure
# X is a matrix of firm characteristics
# Note: They all should have the same number of rows. 
lm.for.amm.exposure <- function(exposures, var.exposures, X) {
  m.ols <- lm(exposures ~ -1 + X)  
# Likelihood function for OLS-by-MLE where each obs has a known standard error --
  mine.lf <- function(theta, y, X, y.se2) {
    if (theta[1] <= 0) return(NA)         # Refuse to look at -ve sigma.
    -sum(dnorm(y, mean= X %*% theta[-1], sd=sqrt(theta[1]+y.se2), log=TRUE))
  }  
 ## Use OLS as starting values for the MLE --
  seeds <- c(summary(m.ols)$sigma, coef(m.ols))
  p <- optim(seeds, method="L-BFGS-B", fn=mine.lf, hessian=TRUE,
             lower=c(1e-2, -Inf, -Inf, -Inf), upper=rep(Inf, 4),
             y=exposures, X=X, y.se2=var.exposures)
  inverted <- solve(p$hessian)
 # Now get to generation of a nicely organised matrix collecting up the results.
 # First setup a clean set of OLS results --
  results <- summary(m.ols)$coefficients[,c(1,3)]
  rownames(results) <- substr(rownames(results), 2, 1000)
  colnames(results) <- c("OLS estimates", "OLS t stats")
  results <- rbind(results, c(summary(m.ols)$sigma, "NA"))
  rownames(results)[nrow(results)] <- "Sigma"
# Now augment it on the right with the MLE --
  tmp <- p$par/sqrt(diag(inverted))
  results <- cbind(results, cbind(c(p$par[-1], p$par[1]),
                                  c(tmp[-1], tmp[1])))
  colnames(results) <- c(colnames(results)[1:2], "MLE estimates", "MLE t stats")
  results
}


####################################################

print.amm <-
function(amm, verbose=FALSE) {
  if (verbose) {print(summary(amm$lm.res))}
  cat("Summary statistics of exposure:\n")
  sstats <- cbind(amm$exposure, amm$s.exposure,
                  amm$exposure/amm$s.exposure)
  colnames(sstats) <- c("Exposure", "Std.Err", "t statistic")  
  rownames(sstats) <- names(amm$exposures)
  print(sstats)
  return(0)
}

################################################################################
# You got to write a plot function that does cool stuff with the AMM results! 
################################################################################

simulated.mean <- function (beta, stdev, fun=NULL, no.of.draws){
    na1 <- is.na(beta)
    na2 <- is.na(stdev)
   if (!identical(na1, na2)) {
        cat("Panic - two is.na() are not identical.\n")
       return(-1)
   }
    b <- beta[!is.na(beta)]
   s <- stdev[!is.na(stdev)]
    N <- length(b)
    draws <- NULL
    for (i in 1:no.of.draws) {
     if(!is.null(fun)){
        draws <- c(draws, mean(fun((rnorm(N) * s) + b)))
      }else{
       draws <- c(draws, mean(rnorm(N)*s+b))
     }
  }
    draws
}

#########################################
kernel.plots <- function(draws, logscale=NULL) {
    nplots <- ncol(draws)
      hilo <- range(draws)
      par(mfrow=c(nplots,1), mai=c(.4,.8,.2,.2))
      for (i in 1:nplots) {
        if(!is.null(logscale)){
            plot(density(draws[,i]), main=colnames(draws)[i], xlab="",
                          col="blue", xlim=hilo, log=logscale,lwd=2)
          }else{
	    plot(density(draws[,i]), main=colnames(draws)[i], xlab="",
                          col="blue", xlim=hilo, lwd=2)
          }
	}
  }
