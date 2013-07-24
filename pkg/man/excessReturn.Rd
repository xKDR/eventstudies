\name{excessReturn}
\alias{excessReturn}

\title{A function that estimates excess return}

\description{ This function estimates excess return. If the the firm return is rj and market return is rM then output will be rj less rM.
}

\usage{excessReturn(data.object, market.name=NULL)
}

\arguments{
  \item{data.object}{This is a time series object with firm return and market return}
  \item{market.name}{It is the market (index) column name in the data object}

}
\value{Excess market return}

\author{Vikram Bahure}

\examples{ 
data(StockPriceReturns)
er.result <- excessReturn(market.name="nifty",
                          data.object=StockPriceReturns)

}

\keyword{excessReturn}