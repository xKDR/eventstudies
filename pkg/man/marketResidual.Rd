\name{marketResidual}
\alias{marketResidual}

\title{This function extracts market return from the firm return }

\description{ This function extracts market return using regression from the firm return to get the residual return
}

\usage{marketResidual(data.object, market.name=NULL)
}

\arguments{
  \item{data.object}{Single time series object with firm/firms and stock index data}
  \item{market.name}{Column name of the market index in the data}

}
\value{ Market residual after extracting market returns from the firm return
}

\author{Vikram Bahure}

\examples{ 
data(StockPriceReturns)
# Extracting market residual
mm.result <- marketResidual(data.object=StockPriceReturns, market.name="nifty")

}

\keyword{marketResidual}