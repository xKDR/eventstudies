\name{lmAMM}
\alias{lmAMM}

\title{Augmented market model (AMM) estimation}

\description{This function relates firm returns to the regressors using the linear model (market index movements, currency fluctuations etc.).
It computes firm exposure for all the regressors in columns of X and subsequently estimates AMM residuals by purging out variation
from regressand.
}

\usage{
lmAMM(firm.returns, X, nlags = NA, verbose = FALSE)
}

\arguments{
  \item{firm.returns}{A time series object of firm returns for AMM estimation}

  \item{X}{A time series of explanatory variables obtained from
    the makeX function. The first variable is always the
    stock market index. Other variables could be
    such as currency or bond returns, or other variables as
  desired by the user. 
  }

  \item{nlags}{Number of lags of explanatory variables. When unspecified
    the best lag using the AIC is used.
  }

  \item{verbose}{Default is FALSE. When set to TRUE, the function
    prints detailed results while running this function. 
  }
}


\value{ The function returns the exposures, HAC adjusted standard
  errors, the number of lags used, and the residuals from the fitted
  model.

\item{exposures}{This contains the exposure estimates for the firm
j}

\item{s.exposures}{This contains the HAC adjusted standard error
of the exposures estimated for the firm firm.returns.}

\item{nlags}{Specifies the lag length provided by the user}

}

\author{Ajay Shah, Vimal Balasubramaniam}

\seealso{
\code{\link{manyfirmsAMM}, \link{makeX}}
}

\examples{
# lmAMM
data("lmAMMData")
firm.returns <- lmAMMData[,c("Infosys")]
market.returns <- lmAMMData[,"index.nifty"]
currency.returns <- lmAMMData[,"currency.inrusd"]
## Creating regressors for AMM estimation using makeX function
X <- makeX(market.returns, others=currency.returns,
           switch.to.innov=FALSE, market.returns.purge=FALSE, verbose=FALSE)
a <- lmAMM(firm.returns, X, nlags=0, verbose=FALSE)
print(a)
}

\keyword{lmAMM}