\name{marketResidual}
\alias{marketResidual}

\title{This function extracts market return from the firm return }

\description{ This function extracts market return using regression from the firm return to get the residual return
}

\usage{marketResidual(firm.returns, market.returns)
}

\arguments{
  \item{firm.returns}{Single time series object with all firm returns}
  \item{market.returns}{Market index returns}

}
\value{ Market residual after extracting market returns from the firm return
}

\author{Vikram Bahure}

\examples{ 
data(StockPriceReturns)
data(nifty.index)
# Extracting market residual
mm.result <- marketResidual(firm.returns=StockPriceReturns, market.returns=nifty.index)

}

\keyword{marketResidual}
