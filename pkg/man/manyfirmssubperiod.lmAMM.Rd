\name{manyfirmssubperiod.lmAMM}
\alias{manyfirmssubperiod.lmAMM}

\title{Compute AMM for multiple firms across several periods}

\description{This function runs AMM for multiple firms at once using
  the matrix of data obtained from \code{makeX}, and a matrix of LHS
  variables.}

\usage{
manyfirmssubperiod.lmAMM(firm.returns,
                         X,
                         lags,
                         dates = NULL,
                         periodnames = NULL,
                         verbose = FALSE)
}

\arguments{

  \item{firm.returns}{A \pkg{zoo} object of all firms on which AMM is to be
    run.}

  \item{X}{a \pkg{zoo} object containing at least two regressors
    generally obtained after applying the \dQuote{makeX} function.}

  \item{lags}{an integer specifying the number of lags to be used in the
    market model.}

  \item{dates}{a set of dates that mark out subperiods of interest. If
    dates is \sQuote{NULL}, then full period is considered.}

  \item{periodnames}{a \sQuote{character} vector of names for each
    subperiod that has been marked by the \dQuote{dates} argument.}

  \item{verbose}{Whether to print detailed output. Default is \sQuote{FALSE}}
}

%% FIXME: \details section!
%% steps:
%% - calculates exposures and SDS
%% - runs AMM for each subperiod for each firm

\section{Warning}{Do not have any space between names provided under
  \dQuote{periodnames}.}

\author{Ajay Shah, Vimal Balasubramaniam}

\seealso{
\code{\link{lmAMM}}
}

\examples{

## Running manyfirmssubperiod.lmAMM() involves as many steps as working
## with onefirmAMM.
data("lmAMMData")

firm.returns <- lmAMMData[, c("Infosys","TCS")]
market.returns <- lmAMMData[, "index.nifty"]
currency.returns <- lmAMMData[, "currency.inrusd"]

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
