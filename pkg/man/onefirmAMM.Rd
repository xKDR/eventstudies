\name{subperiod.lmAMM}
\alias{subperiod.lmAMM}

\title{A function that estimates exposure for a single firm over multiple periods}

\description{This function typically utilises an firm.returns vector
  and an X matrix of explanatory variables obtained out of
  using the makeX function. This would compute the exposure
  for all columns in X for the specified time periods. 
}

\usage{
subperiod.lmAMM(firm.returns, X, nlags = 1, verbose = FALSE, dates = NULL, residual = TRUE)
}

\arguments{
  \item{firm.returns}{A vector of data for one firm
  }
  \item{X}{A matrix of explanatory variables obtained from
    the makeX function. The first variable is always the
    stock market index. Other variables could be risk factors
    such as currency or bond returns, or foreign portfolio inflows.
  }
  \item{nlags}{Number of lags of explanatory variables. When unspecified
    the best lag using the AIC is used.
  }
  \item{verbose}{Default is FALSE. When set to TRUE, the function
    prints detailed results of using the function. 
  }
  \item{dates}{Default is NULL. If no dates are mentioned, subperiod.lmAMM does
  what firmExposures() would do, i.e., estimate exposures for the full time period.
  }
  \item{residual}{Returns AMM Residuals if TRUE, AMM exposure
  otherwise. Defaults to TRUE.}
}
\value{ The function returns the exposures, HAC adjusted standard
  errors, the number of lags used, and the residuals from the fitted
  model.}

\author{Vimal Balasubramaniam}

\seealso{ \code{\link{firmExposures}},
\code{\link{manyfirmsAMM}}}

\examples{ 
# Create RHS before running subperiod.lmAMM()
data("lmAMMData")
firm.returns <- lmAMMData$Infosys
market.returns <- lmAMMData$index.nifty
currency.returns <- lmAMMData$currency.inrusd
regressors <- makeX(market.returns, others=currency.returns,
              switch.to.innov=TRUE, market.returns.purge=TRUE, nlags=1,
              dates=as.Date(c("2012-02-01","2013-01-01","2014-01-31")), verbose=FALSE)
# Run AMM for one firm across different periods
res <- subperiod.lmAMM(firm.returns,
		 X=regressors,
                 nlags=1,
            	 verbose=TRUE,
            	 dates= as.Date(c("2012-02-01","2013-01-01","2014-01-31")))
}

\keyword{subperiod.lmAMM}