\name{lmAMM}
\alias{lmAMM}

\title{Augmented market model (AMM) estimation}

\description{This function relates firm returns to the regressors using
  the linear model (market index movements, currency fluctuations etc.).
  It computes firm exposure for all the regressors in columns of \sQuote{X} and
  subsequently estimates AMM residuals by purging out variation from
  the regressand.}

\usage{
lmAMM(firm.returns, X, nlags = NA, verbose = FALSE)
}

\arguments{

  \item{firm.returns}{a \pkg{zoo} time series object of firm returns for
    AMM estimation.}

  \item{X}{a time series of explanatory variables obtained from the
    \sQuote{makeX} function. The first variable is always the stock market
    index. Other variables could be such as currency or bond returns, or
    other variables as desired by the user.}

  \item{nlags}{an \sQuote{integer} with the number of lags of
    explanatory variables. When unspecified, the best lag using the AIC
    is used.}

  \item{verbose}{a \sQuote{logical}. Print function details. When set to
    \sQuote{TRUE}, the function prints detailed results while
    running. Default is \sQuote{FALSE}.}  }


\value{The function returns an object of \sQuote{class} \dQuote{amm}.

  The functions \sQuote{summary} and \sQuote{print} are provided to
  print a summary of results and print the command, coefficients, and
  exposures of the analysis. The function \sQuote{plot} is provided to
  plot the model residuals and firm returns on the same graph.

  An object of class \dQuote{amm} is a \sQuote{list} containing
  the result of \code{stats::lm} function along with the following
  components:

  \item{exposures}{a \sQuote{numeric} containing exposure estimates for
    the firm j.}

  \item{s.exposures}{a \sQuote{numeric} containing the HAC adjusted
    standard error of the exposures estimated for the firm
    with returns \sQuote{firm.returns}.}

  \item{nlags}{shows the lag length provided by user.}
}

\author{Ajay Shah, Vimal Balasubramaniam}

\seealso{
  \code{\link{makeX}} for preparing explanatory variables for
  computation of Augmented Market Models used in this function.
}

\examples{
data("lmAMMData")

firm.returns <- lmAMMData[,"Infosys"]
market.returns <- lmAMMData[,"index.nifty"]
currency.returns <- lmAMMData[,"currency.inrusd"]

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
