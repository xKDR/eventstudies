###############################################
# Estimating one firm's exposure in one period.
###############################################
firmExposures <- function(rj, X, nlags=NA, verbose=FALSE) {
  do.ols <- function(nlags) {
    tmp <- cbind(rj, X[,1]) # Assume 1st is stock index, and no lags are required there.
    labels <- c("rj","rM1")
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
    lm(rj ~ ., data=as.data.frame(tmp))
  }

  if (is.na(nlags)) {
    if (verbose) {cat("Trying to find the best lag structure...\n")}
    bestlag <- 0
    bestm <- NULL
    bestAIC <- Inf
    for (trylag in 0:min(10,log10(length(rj)))) {
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
  # First the rM1
  exposures <- beta[2]                  # no lags for rM1
  s.exposures <- sqrt(Sigma[2,2])
  # From here on, there's a block of 1+nlags coeffs for each
  # of the non-rM1 regressors.
  if (NCOL(X) > 1) {
    for (i in 2:NCOL(X)) {
      n.block1 <- 2 + ((i-2)*(1+nlags)) # Just 2 for the 1st case.
      n.block2 <- length(beta) - n.block1 - (1 + nlags)
      w <- c(rep(0, n.block1), rep(1, 1+nlags), rep(0, n.block2))
      exposures <- c(exposures, w %*% beta)
      s.exposures <- c(s.exposures, sqrt(w %*% Sigma %*% w))
    }
  }
  names(exposures) <- names(s.exposures) <- colnames(X)
  results <- list(exposures=exposures,
                  s.exposures=s.exposures, nlags=nlags,
                  lm.res=m)
  class(results) <- "amm"
  results
}

