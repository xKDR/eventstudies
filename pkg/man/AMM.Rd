\name{AMM}
\alias{AMM}

\title{A function that estimates exposure for a single firm over multiple periods}

\description{This function does three tasks: One firm AMM model, many firms AMM model and firm exposures. One should refer to the individual functions manual for arguments. 
}

\usage{
AMM(amm.type=NULL, ...)
}

\arguments{
  \item{amm.type}{If amm.type take following inputs: all, residual and firmExposures. If amm.type is residual it gives only AMM residual as output, if amm.type is firmExposures then it gives only firm exposure as output else if all output is needed then option all is used.}
  \item{...}{Accepts specific arguments for the model.}
}
\value{The function returns the exposures, HAC adjusted standard
  errors, the number of lags used, and the residuals from the fitted
  model. Default is NULL.

\item{exposures}{This contains the exposure estimates for the firm
j}

\item{s.exposures}{This contains the HAC adjusted standard error
of the exposures estimated for the firm rj.}

\item{nlags}{Specifies the lag length provided by the user}

\item{lm.res}{The model estimates used in the analysis are stored here.}
}

\author{Vikram Bahure}

\seealso{ \code{\link{firmExposures}},
\code{\link{onefirmAMM}}}

\examples{ 
# Create RHS before running AMM()
data("y3c3")
NIFTY_INDEX <- y3c3$NIFTY_INDEX
INRUSD <- y3c3$INRUSD
Company_A <- y3c3$Company_A
Company_B <- y3c3$Company_B
Company_C <- y3c3$Company_C
regressand <- cbind(Company_A,Company_B,Company_C)

## One firm
of <- AMM(amm.type="residual",rj=Company_A,
            verbose=TRUE,
            dates= as.Date(c("2005-01-15","2006-01-07","2007-01-06",
                       "2008-01-05","2009-01-03")),
           rM1=NIFTY_INDEX, others=INRUSD,
           switch.to.innov=TRUE, rM1purge=TRUE, nlags=1)
}

\keyword{AMM}