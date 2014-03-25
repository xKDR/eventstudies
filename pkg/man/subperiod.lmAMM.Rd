\name{subperiod.lmAMM}
\alias{subperiod.lmAMM}

\title{Function to estimate exposure for a single firm over multiple periods}

\description{This function typically utilises a firm.returns vector and
  an X matrix of explanatory variables obtained out of the
  \sQuote{makeX} function. It computes the exposure for all columns in X
  for the specified time periods.
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
  \item{firm.returns}{a \sQuote{numeric} vector of data for one firm.
  }
  \item{X}{a matrix of explanatory variables obtained from
    the \sQuote{makeX()} function. The first variable is always the
    stock market index. Other variables could be risk factors
    such as currency or bond returns, or foreign portfolio inflows.
  }
  \item{nlags}{\sQuote{integer} of length 1, number of lags of explanatory
  variables. When unspecified, the best lag using the AIC is used.
  }
  \item{verbose}{\sQuote{logical}, indicating whether the function
    should print detailed results.
  }
  \item{dates}{object of \sQuote{Date} class, specifying the time period
  to estimate exposures for. Default is \sQuote{NULL}. If no dates are
  mentioned, \sQuote{subperiod.lmAMM} does what \sQuote{firmExposures()}
  would do, i.e., estimate exposures for the full time period.
  }
  \item{residual}{\sQuote{logical}, returns AMM residuals if TRUE, AMM
  exposure otherwise. Defaults to \sQuote{TRUE}.}
}
\value{A \sQuote{list} of length 3 is returned:
  \item{exposures}{AMM exposure}
  \item{sds}{HAC adjusted standard errors}
  \item{residuals}{an \sQuote{xts} object of the residuals from the fitted model}
}

\author{Vimal Balasubramaniam}

\seealso{ \code{\link{lmAMM}},
\code{\link{manyfirmssubperiod.lmAMM}}}

\examples{ 
data("lmAMMData")

## Create RHS before running subperiod.lmAMM()
firm.returns <- lmAMMData$Infosys
market.returns <- lmAMMData$index.nifty
currency.returns <- lmAMMData$currency.inrusd

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
}

\keyword{subperiod.lmAMM}