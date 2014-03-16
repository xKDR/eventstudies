\name{lmAMM}
\alias{lmAMM}

\title{Augmented market model (AMM) estimation}

\description{This function relates firm returns to the regressors using
  the linear model (market index movements, currency fluctuations etc.).
  It computes firm exposure for all the regressors in columns of X and
  subsequently estimates AMM residuals by purging out variation from
  the regressand.}

\usage{
lmAMM(firm.returns, X, nlags = NA, verbose = FALSE)
}

\arguments{

  \item{firm.returns}{a \pkg{zoo} time series object of firm returns for
    AMM estimation.}

  \item{X}{a time series of explanatory variables obtained from the
    makeX function. The first variable is always the stock market
    index. Other variables could be such as currency or bond returns, or
    other variables as desired by the user.}

  \item{nlags}{an \sQuote{integer} with the number of lags of
    explanatory variables. When unspecified, the best lag using the AIC
    is used.}

  \item{verbose}{a \sQuote{logical}. Print function details. When set to
    \sQuote{TRUE}, the function prints detailed results while
    running. Default is \sQuote{FALSE}.}  }


\value{The function returns a \sQuote{list} containing exposures, HAC
  adjusted standard errors, number of lags used, and residuals from the
  fitted model.

  \item{exposures}{a \sQuote{numeric} containing exposure estimates for
    the firm j.}

  %% FIXME: "for the firm firm.returns" doesn't make sense. Is it "for
  %% the firm with returns 'firm.returns'?
  \item{s.exposures}{a \sQuote{numeric} containing the HAC adjusted
    standard error of the exposures estimated for the firm
    firm.returns.}

  \item{nlags}{shows the lag length provided by user.}
}

\author{Ajay Shah, Vimal Balasubramaniam}

\seealso{
\code{\link{manyfirmsAMM}, \link{makeX}}
}

\examples{
# lmAMM
data("lmAMMData")
firm.returns <- lmAMMData[,"Infosys"]
market.returns <- lmAMMData[,"index.nifty"]
currency.returns <- lmAMMData[,"currency.inrusd"]
## Creating regressors for AMM estimation using makeX function
X <- makeX(market.returns, others=currency.returns,
           switch.to.innov=FALSE, market.returns.purge=FALSE, verbose=FALSE)
a <- lmAMM(firm.returns, X, nlags=0, verbose=FALSE)
print(a)
}

\keyword{lmAMM}