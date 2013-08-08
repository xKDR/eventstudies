\name{firmExposures}
\alias{firmExposures}

\title{A function that estimates exposure for a single firm in a
single period}

\description{This function typically utilises an rj vector
  and an X matrix of explanatory variables obtained 
  using the makeX function. This would compute the exposure
  for all columns in X.
}

\usage{
firmExposures(rj, X, nlags = NA, verbose = FALSE)
}

\arguments{
  \item{rj}{A vector of data for one firm}

  \item{X}{A matrix of explanatory variables obtained from
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
of the exposures estimated for the firm rj.}

\item{nlags}{Specifies the lag length provided by the user}

\item{lm.res}{The model estimates used in the analysis are stored here.}

}

\author{Ajay Shah, Vimal Balasubramaniam}

\seealso{
\code{\link{manyfirmsAMM}}
}

\examples{
# firmExposures
data("firmExposuresData")
rj  <- firmExposuresData$Company_A
market.returns <- firmExposuresData$NIFTY_INDEX
rM2 <- firmExposuresData$usdinr
rM3 <- firmExposuresData$baa
X <- makeX(market.returns, others=rM2,
           switch.to.innov=FALSE, market.returns.purge=FALSE, verbose=FALSE)
a <- firmExposures(rj, X, nlags=0, verbose=FALSE)
print(a)
}

\keyword{firmExposures}