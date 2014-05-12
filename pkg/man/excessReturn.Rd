\name{excessReturn}
\alias{excessReturn}

\title{Estimate excess returns over the market}

\description{
  This function estimates excess returns over the market. If the firm
  return is \dQuote{firm.returns} and market return is
  \dQuote{market.returns}, then output will be \dQuote{firm.returns}
  less \dQuote{market.returns}.
}

\usage{excessReturn(firm.returns, market.returns)}

\arguments{
  \item{firm.returns}{a \pkg{zoo} timeseries with firm returns from
    which excess returns from market are to be calculated.}
  
  \item{market.returns}{a \pkg{zoo} object containing market index
    returns.}
}
\value{Excess market return}

\author{Vikram Bahure}

\examples{ 
data(StockPriceReturns)
data(OtherReturns)

er.result <- excessReturn(firm.returns = StockPriceReturns,
			  market.returns = OtherReturns$NiftyIndex)

tail(merge(excessReturn = er.result$Infosys,
           Infosys = StockPriceReturns$Infosys,
           NiftyIndex = OtherReturns$NiftyIndex,
           all=FALSE))
}

\keyword{excessReturn}
