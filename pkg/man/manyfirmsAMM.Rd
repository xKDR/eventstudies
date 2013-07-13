\name{manyfirmsAMM}
\alias{manyfirmsAMM}

\title{A function to compute AMM for multiple firms across several
periods}

\description{
  This function will compute AMM for multiple firms at once using the
  matrix of data obtained from \code{makeX}, and a matrix of LHS variables}

\usage{
manyfirmsAMM(regressand, regressors, lags, dates = NULL, periodnames = NULL, verbose = FALSE)
}

\arguments{
  \item{regressand}{A zoo matrix of all firms for which AMM is to
  be computed}

  \item{regressors}{A zoo matrix containing at least two
  regressors generally obtained after applying the makeX function }

  \item{lags}{ Specifies the number of lags to be used in the
  market model} 

  \item{dates}{A set of dates that mark out subperiods of
  interest} 

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
# Running manyfirmsAMM() involves as many steps as working with onefirmAMM. 

data("ammData",package="amm")       
NIFTY_INDEX <- y3c3$NIFTY_INDEX
INRUSD <- y3c3$INRUSD
Company_A <- y3c3$Company_A
Company_B <- y3c3$Company_B
Company_C <- y3c3$Company_C
regressors <- makeX(rM1=NIFTY_INDEX, others=INRUSD,
           switch.to.innov=TRUE, rM1purge=TRUE, nlags=1,
           dates=as.Date(c("2005-01-15","2006-01-07","2007-01-06",
                       "2008-01-05","2009-01-03")), verbose=FALSE)
regressand <- cbind(Company_A,Company_B,Company_C)
res <- manyfirmsAMM(regressand,regressors,lags=1,
                     dates=as.Date(c("2005-01-15","2006-01-07","2007-01-06",
                       "2008-01-05","2009-01-03")),periodnames=c("P1","P2","P3","P4"),
                     verbose=FALSE)

}

\keyword{manyfirmsAMM}