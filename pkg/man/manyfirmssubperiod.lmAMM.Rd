\name{manyfirmssubperiod.lmAMM}
\alias{manyfirmssubperiod.lmAMM}

\title{A function to compute AMM for multiple firms across several
periods}

\description{
  This function will compute AMM for multiple firms at once using the
  matrix of data obtained from \code{makeX}, and a matrix of LHS variables}

\usage{
manyfirmssubperiod.lmAMM(firm.returns, X, lags, dates = NULL, periodnames = NULL, verbose = FALSE)
}

\arguments{
  \item{firm.returns}{A zoo matrix of all firms for which AMM is to
  be computed}

  \item{X}{A zoo matrix containing at least two
  regressors generally obtained after applying the makeX function }

  \item{lags}{ Specifies the number of lags to be used in the
  market model} 

  \item{dates}{A set of dates that mark out subperiods of
  interest. If dates is NULL then full period is considered.} 

  \item{periodnames}{Name for each subperiod that has been marked
  by the dates above.} 

  \item{verbose}{Whether detailed print while running this
  function is required} 
}

\section{Warning}{Do not have any space between names provided under 'periodnames'}

\author{Ajay Shah, Vimal Balasubramaniam}

\seealso{
\code{\link{lmAMM}}
}

\examples{
# Running manyfirmssubperiod.lmAMM() involves as many steps as working with onefirmAMM. 
data("lmAMMData")
firm.returns <- lmAMMData[,c("Infosys","TCS")]
market.returns <- lmAMMData[,"index.nifty"]
currency.returns <- lmAMMData[,"currency.inrusd"]
## Creating X for AMM estimation using makeX function
X <- makeX(market.returns, others=currency.returns, nlags=1,
              switch.to.innov=FALSE, market.returns.purge=FALSE, verbose=FALSE,
	      dates=as.Date(c("2012-02-01","2013-01-01","2014-01-20")))
## Estimating exposure
res <- manyfirmssubperiod.lmAMM(firm.returns,X,lags=1,
                     dates=as.Date(c("2012-02-01","2013-01-01","2014-01-20")),
		     periodnames=c("P1","P2"),
                     verbose=FALSE)
print(res)
}

\keyword{manyfirmssubperiod.lmAMM}