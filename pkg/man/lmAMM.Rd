\name{lmAMM}
\alias{lmAMM}

\title{Augmented market model (AMM) estimation}

\description{\sQuote{lmAMM} is used to estimate augmented market
  model. It can be used to estimate AMM and calculate augmented market
  model residuals (AMM abnormal returns) stripped of market and macro
  variations.}

\usage{
lmAMM(firm.returns, X, nlags = NA, verbose = FALSE)
}

\arguments{

  \item{firm.returns}{a \pkg{zoo} time series object of firm returns for
    AMM estimation.}

  \item{X}{a time series of explanatory variables obtained from the
    \sQuote{makeX} function. See \sQuote{Details}.}

  \item{nlags}{an \sQuote{integer} with the number of lags of
    explanatory variables.}

  \item{verbose}{\sQuote{logical}. Print function details. If
    \sQuote{TRUE}, print detailed results while running. Default is
    \sQuote{FALSE}.}

}

\details{
  The function relates firm returns to the regressors (market
  index movements, currency fluctuations etc.) using the linear model
  \sQuote{lm}.  It computes firm exposure for all the regressors in
  columns of \dQuote{X} and subsequently estimates AMM residuals by
  purging out variation from the regressand.

  Function \sQuote{makeX} is used to prepare the explanatory variables
  (\dQuote{X}) used in the estimation of AMM. The first variable of
  \dQuote{X} is always the stock market index. Other variables could be
  currency, bond returns, or other variables as required.

  If \dQuote{nlags} is \sQuote{NA} (default), then the function tries to
  find the best lag structure using the AIC.

  \sQuote{lmAMM} calls the \code{stats::lm} to do the
  OLS. \sQuote{print} function on an object of \sQuote{class}
  \dQuote{amm} can be used to see the call (formula) to \code{lm}.
}

\value{The function returns an object of \sQuote{class} \dQuote{amm}.

  The functions \sQuote{summary} and \sQuote{print} are provided to
  print a summary of results and print the \sQuote{lm} command,
  coefficients, and exposures of the analysis. The function
  \sQuote{plot} is provided to plot the model residuals and firm
  returns.

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
  \code{\link{makeX}}
  \code{\link{lm}}
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
