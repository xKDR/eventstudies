\name{onefirmAMM}
\alias{onefirmAMM}

\title{A function that estimates exposure for a single firm over multiple periods}

\description{This function typically utilises an rj vector
  and an X matrix of explanatory variables obtained out of
  using the makeX function. This would compute the exposure
  for all columns in X for the specified time periods. 
}

\usage{
onefirmAMM(rj, X, nlags = 1, verbose = FALSE, dates = NULL, residual = TRUE)
}

\arguments{
  \item{rj}{A vector of data for one firm
  }
  \item{X}{A matrix of explanatory variables obtained from
    the makeX function. The first variable is always the
    stock market index. Other variables could be risk factors
    such as currency or bond returns, or foreign portfolio inflows.
  }
  \item{nlags}{Number of lags of explanatory variables. When unspecified
    the best lag using the AIC is used.
  }
  \item{verbose}{Default is FALSE. When set to TRUE, the function
    prints detailed results of using the function. 
  }
  \item{dates}{Default is NULL. If no dates are mentioned, onefirmAMM does
  what firmExposures() would do, i.e., estimate exposures for the full time period.
  }
  \item{residual}{Returns AMM Residuals if TRUE, AMM exposure
  otherwise. Defaults to TRUE.}
}
\value{ The function returns the exposures, HAC adjusted standard
  errors, the number of lags used, and the residuals from the fitted
  model.}

\author{Vimal Balasubramaniam}

\seealso{ \code{\link{firmExposures}},
\code{\link{manyfirmsAMM}}}

\examples{ 
# Create RHS before running onefirmAMM()
data("y3c3")
NIFTY_INDEX <- y3c3$NIFTY_INDEX
INRUSD <- y3c3$INRUSD
Company_A <- y3c3$Company_A
rhs.dat <- makeX(NIFTY_INDEX, others=INRUSD,
           switch.to.innov=TRUE, rM1purge=TRUE, nlags=1,
           dates=as.Date(c("2005-01-15","2006-01-07","2007-01-06",
                       "2008-01-05","2009-01-03")), verbose=FALSE)

# Run AMM for one firm across different periods
onefirmAMM(rj=Company_A,
            X=rhs.dat,
            nlags=1,
            verbose=TRUE,
            dates= as.Date(c("2005-01-15","2006-01-07","2007-01-06",
                       "2008-01-05","2009-01-03")))
}

\keyword{onefirmAMM}