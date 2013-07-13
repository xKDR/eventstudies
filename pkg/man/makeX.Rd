\name{makeX}
\alias{makeX}
\title{A function to prepare explanatory variables for computation
of Augmented Market Models
}

\description{
  This function creates a matrix of explanatory variables in the
  form specified by the user for further estimation of Augmented
  market models
}

\usage{
makeX(rM1, others, switch.to.innov = rep(TRUE, NCOL(others)), rM1purge = TRUE, nlags = 5, dates = NULL, verbose = TRUE)
}

\arguments{
  \item{rM1}{This is generally fixed to the stock market index. The
  first column vector of this matrix contains this variable. 
}

  \item{others}{A zoo matrix with other regressors of interest in the
  AMM - this could be currency, bond returns, foreign flows or any
  other variable.  
}

  \item{switch.to.innov}{is a vector of booleans with one element for
    each column in 'others'. This controls whether or not that column in
    'others' is switched from raw values to AR residuals. 
}

  \item{rM1purge}{whether or not the effects of all these 'others' are
  purged from 'rM1' (i.e., is it replaced by residuals of a model
  explaining rM1 using all these 'others'). 
}

  \item{nlags}{The number of lag terms present in this model explaining
  rM1 using all these 'others'.
}

  \item{dates}{Specified break dates (either from structural breaks in
  exchange rate regimes) so that all these steps are constructed within
  each sub-period divided by the dates.
}

  \item{verbose}{Whether detailed print while running this function
  is required.
}

}

\section{Warning}{The input data has to be all na.omit(). There
  should be no NAs in the dataset fed into this function. 
}

\value{This function generates a user specified matrix of explanatory
  variables that will be further used in running Augmented market
  models. 
}


\author{Ajay Shah, Vimal Balasubramaniam}

\seealso{ \code{\link{do.one.piece}}}

\examples{
# makeX
data("firmExposuresData")
rj  <- test$Company_A
rM1 <- test$NIFTY_INDEX
rM2 <- test$usdinr
rM3 <- test$baa
X <- makeX(rM1, others=rM2,
           switch.to.innov=FALSE, rM1purge=FALSE, verbose=FALSE)
}

\keyword{makeX}
\keyword{do.one.piece}