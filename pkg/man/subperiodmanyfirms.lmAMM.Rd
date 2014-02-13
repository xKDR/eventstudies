\name{subperiodmanyfirms.lmAMM}
\alias{subperiodmanyfirms.lmAMM}

\title{A function to compute AMM for multiple firms across several
periods}

\description{
  This function will compute AMM for multiple firms at once using the
  matrix of data obtained from \code{makeX}, and a matrix of LHS variables}

\usage{
subperiodmanyfirms.lmAMM(regressand, regressors, lags, dates = NULL, periodnames = NULL, verbose = FALSE)
}

\arguments{
  \item{regressand}{A zoo matrix of all firms for which AMM is to
  be computed}

  \item{regressors}{A zoo matrix containing at least two
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
\code{\link{onefirmAMM}}
}

\examples{
# Running subperiodmanyfirms.lmAMM() involves as many steps as working with onefirmAMM. 
data("lmAMMData")
regressand <- lmAMMData[,c("Infosys","TCS")]
market.returns <- lmAMMData[,"index.nifty"]
currency.returns <- lmAMMData[,"currency.inrusd"]
## Creating regressors for AMM estimation using makeX function
regressors <- makeX(market.returns, others=currency.returns, nlags=1,
              switch.to.innov=FALSE, market.returns.purge=FALSE, verbose=FALSE,
	      dates=as.Date(c("2012-02-01","2013-01-01","2014-01-31")))
## Estimating exposure
res <- subperiodmanyfirms.lmAMM(regressand,regressors,lags=1,
                     dates=as.Date(c("2012-02-01","2013-01-01","2014-01-31")),
		     periodnames=c("P1","P2"),
                     verbose=FALSE)
print(res)
}

\keyword{subperiodmanyfirms.lmAMM}