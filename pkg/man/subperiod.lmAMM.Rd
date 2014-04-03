\name{subperiod.lmAMM}
\alias{subperiod.lmAMM}

\title{Estimate exposure for a single regressor over multiple periods}

\description{This function estimates exposure for a single regressor
  over a set of regressands obtained by using \sQuote{makeX}
  over multiple periods.
}


\usage{
subperiod.lmAMM(firm.returns,
                X,
                nlags = 1,
                verbose = FALSE,
                dates = NULL,
                residual = TRUE)
}

\arguments{
  \item{firm.returns}{a \sQuote{zoo} vector of data for one
    regressor (firm).
  }

  \item{X}{a matrix of regressands obtained by using \sQuote{makeX}. 
    See \sQuote{Details} when this is specified as a market model. 
  }
  
  \item{nlags}{specifies a lag length required from the specified set
    of regressands. When unspecified, the best lag using the AIC is used.
  }

  \item{verbose}{\sQuote{logical}, indicating whether the function
    should print detailed results.
  }

  \item{dates}{ a \sQuote{Date} class vector, specifying break points in
    the time series to be used for sub-period identification.
    The default value is \sQuote{NULL} resulting in estimates
    identical to \sQuote{lmAMM}.
  }
  
  \item{residual}{\sQuote{logical}, returns AMM residuals if TRUE, AMM
    exposure otherwise. Defaults to \sQuote{TRUE}.}
}

\details{
  When \sQuote{dates} is \sQuote{NULL}, resulting estimates from this
  function is identical to \sQuote{lmAMM}.
}

\value{A \sQuote{list} object of length 3 is returned with: 
  \itemize{
    \item{\dQuote{exposures}: A matrix of exposures by sub-period and regressands.}
    \item{\dQuote{sds}: HAC adjusted standard errors for all exposures.}
    \item{\dQuote{residuals}: Contain residuals of class \pkg{xts} from the
      fitted model for all sub-periods.}
  }
}

\author{Vimal Balasubramaniam}

\seealso{ \code{\link{lmAMM}}}

\examples{ 
data("AMMData")

## Create RHS before running subperiod.lmAMM()
firm.returns <- AMMData$Infosys
market.returns <- AMMData$index.nifty
currency.returns <- AMMData$currency.inrusd

regressors <- makeX(market.returns,
                    others = currency.returns,
                    switch.to.innov = TRUE,
                    market.returns.purge = TRUE,
                    nlags = 1,
                    dates = as.Date(c("2012-02-01","2013-01-01","2014-01-20")),
                    verbose = FALSE)

## Run AMM for one firm across different periods
res <- subperiod.lmAMM(firm.returns,
                       X = regressors,
                       nlags = 1,
                       verbose = FALSE,
                       dates = as.Date(c("2012-02-01",
                                         "2013-01-01",
                                         "2014-01-20")))
str(res)
}
\keyword{subperiod.lmAMM}
