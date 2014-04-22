\name{marketResidual}
\alias{marketResidual}

\title{Extract residuals from a market model}

\description{This function extracts residuals from a market model using
  function \code{stats:lm}.}

\usage{marketResidual(firm.returns, market.returns)}

\arguments{

  \item{firm.returns}{a \pkg{zoo} time series object (univariate or
  otherwise) with firm returns.}

  \item{market.returns}{a \pkg{zoo} time series of market index returns.}
}

\value{Residual returns unexplained by market index returns.}

\author{Vikram Bahure}

\examples{ 
data(StockPriceReturns)
data(NiftyIndex)

## Market model residual
mm.result <- marketResidual(firm.returns = StockPriceReturns,
                            market.returns = NiftyIndex)

## Checking output: Comparing market model residual, raw returns, nifty returns
output <- merge(mm.result$Infosys, StockPriceReturns$Infosys, NiftyIndex, 
                all=FALSE)
colnames(output) <- c("market.residual", "raw.returns", "nifty.returns")
tail(output) 

}

\keyword{marketResidual}
