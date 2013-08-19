\name{AMM}
\alias{AMM}

\title{A function that estimates exposure for a single firm over multiple periods}

\description{This function computes AMM residuals}

\usage{
AMM(...)
}

\arguments{
  \item{...}{Accepts specific arguments for the model.}
}
\value{ This function computes AMM residual using onefirmAMM function.
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
of <- AMM(firm.returns=Company_A,
           verbose=FALSE,
           market.returns=NIFTY_INDEX, others=INRUSD,
           switch.to.innov=TRUE, market.returns.purge=TRUE, nlags=1)
}

\keyword{AMM}
