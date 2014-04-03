\name{lmAMM}
\alias{lmAMM}

\title{Augmented market model (AMM) estimation}

\description{\sQuote{lmAMM} (linear augmented market models) estimates
  exposure and residuals.}

\usage{
   lmAMM(firm.returns, X, nlags = NULL, verbose = FALSE)
}

\arguments{
  \item{firm.returns}{a univariate \sQuote{zoo} object of data for one
    regressor (firm).
  }

  \item{X}{a matrix of regressors obtained by using \sQuote{makeX}. 
    See \sQuote{Details} when this is specified as a market model. 
  }
  
  \item{nlags}{specifies a lag length required from the specified set
    of regressors. When unspecified, the best lag using the AIC is used.
  }

  \item{verbose}{\sQuote{logical}. If \sQuote{TRUE}, prints details of
    piece-wise analysis.}
}

\details{
  This function estimates a linear regression model with multiple
  variables using \sQuote{lm}, stores coefficients as
  \sQuote{exposures}, and HAC adjusted standard errors as \sQuote{s.exposures}. 

  This function is the core engine for other functions that estimate
  AMMs. Each regression is expected in this package to have a minimum of
  30 observations, a condition that translates into a month of
  daily data. If the total number of observations is less than 30, the
  function returns \sQuote{NULL}. 
  
  Function \sQuote{makeX} is used to obtain a matrix of regressors used
  as input for \sQuote{X}. 

  If \dQuote{nlags} is \sQuote{NULL}, then the function finds
  the best lag structure using the AIC(n).

  \sQuote{lmAMM} calls \code{stats::lm} to estimate the linear model.
  \sQuote{print} function on an object of \sQuote{class}
  \dQuote{amm} can be used to see the call (formula) to \code{lm}.
}

\value{The function returns an object of \sQuote{class}
  \dQuote{amm}; \sQuote{NULL} if \code{nrow(firm.returns) < 30}.

  Function \sQuote{summary} is provided to print a summary of
  results. \sQuote{print} prints the coefficients and exposures of the
  analysis. \sQuote{plot} plots the model residuals and firm returns.

  An object of class \dQuote{amm} is a \sQuote{list} containing the
  output of \code{stats::lm} (which includes \dQuote{residuals}),
  along with the following components:

  \item{exposures}{a \sQuote{numeric} containing exposure estimates for
    the firm.}

  \item{s.exposures}{a \sQuote{numeric} containing HAC adjusted
    standard error of the exposures estimated for the firm.}

  \item{nlags}{shows the lag length provided by user.}
}

\author{Ajay Shah, Vimal Balasubramaniam}

\seealso{
  \code{\link{makeX}}
  \code{\link{lm}}
}

\examples{
data("AMMData")

firm.returns <- AMMData[,"Infosys"]
market.returns <- AMMData[,"index.nifty"]
currency.returns <- AMMData[,"currency.inrusd"]

## Creating regressors for AMM estimation using makeX function
X <- makeX(market.returns,
          others = currency.returns,
          switch.to.innov = FALSE,
          market.returns.purge = FALSE,
          verbose = FALSE)

a <- lmAMM(firm.returns, X, nlags = 0, verbose = FALSE)

print(a)
}

\keyword{lmAMM}
