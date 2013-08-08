\name{excessReturn}
\alias{excessReturn}

\title{A function that estimates excess return}

\description{ This function estimates excess return. If the the firm return is rj and market return is rM then output will be rj less rM.
}

\usage{excessReturn(firm.returns, market.returns)

\arguments{
  \item{firm.returns}{Firm returns of which excess return from market is to be calculated}
  \item{market.returns}{Market index returns}

}
\value{Excess market return}

\author{Vikram Bahure}

\examples{ 
data(StockPriceReturns)
data(nifty.index)
er.result <- excessReturn(firm.returns = StockPriceReturns,
			  market.returns = nifty.index)

}

\keyword{excessReturn}