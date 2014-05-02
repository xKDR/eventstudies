\name{manyfirmssubperiod.lmAMM}
\alias{manyfirmssubperiod.lmAMM}

\title{Estimate exposure for many regressands over multiple periods}

\description{\code{\link{manyfirmssubperiod.lmAMM}} estimates exposure
  for many regressands over a set of regressors obtained by using
  \sQuote{makeX} over multiple periods.
  }

\usage{
manyfirmssubperiod.lmAMM(firm.returns,
                         X,
                         lags,
                         dates = NULL,
                         periodnames = NULL,
                         verbose = FALSE)
}

\arguments{
  \item{firm.returns}{a \sQuote{zoo} matrix of data for multiple
    regressands (firms).
  }

  \item{X}{a matrix of regressors obtained by using \sQuote{makeX}. 
  }
  
  \item{lags}{an integer specifying the number of lags to be used in the
    market model.}

  \item{dates}{ a \sQuote{Date} class vector, specifying break points in
    the time series to be used for sub-period identification.
    The default value is \sQuote{NULL} resulting in estimates
    identical to \sQuote{lmAMM} used over multiple regressands.
  }

  \item{periodnames}{a \sQuote{character} vector of names for each
    subperiod that has been marked by the \dQuote{dates} argument.}

  \item{verbose}{\sQuote{logical}, indicating whether the function
    should print detailed results.
  }
}

\details{
  This function computes the exposure, and HAC adjusted standard errors
  to linear augmented market models estimated for several regressands
  across multiple periods. 
}

\section{Warning}{Do not have any space between names provided under
  \dQuote{periodnames}.
}

\author{Chirag Anand, Vikram Bahure, Vimal Balasubramaniam}

\seealso{
\code{\link{lmAMM}}
}

\examples{

## Running manyfirmssubperiod.lmAMM() involves as many steps as working
## with onefirmAMM.
data("AMMData")

firm.returns <- AMMData[, c("Infosys","TCS")]
market.returns <- AMMData[, "index.nifty"]
currency.returns <- AMMData[, "currency.inrusd"]

## Creating X for AMM estimation using makeX function
X <- makeX(market.returns,
           others = currency.returns,
           nlags = 1,
           switch.to.innov = FALSE,
           market.returns.purge = FALSE,
           verbose = FALSE,
	   dates = as.Date(c("2012-02-01", "2013-01-01", "2014-01-20")))

## Estimating exposure
res <- manyfirmssubperiod.lmAMM(firm.returns = firm.returns,
                                X = X,
                                lags = 1,
                                dates = as.Date(c("2012-02-01", "2013-01-01", "2014-01-20")),
                                periodnames = c("P1", "P2"),
                                verbose = FALSE)
print(res)
}

\keyword{manyfirmssubperiod.lmAMM}
