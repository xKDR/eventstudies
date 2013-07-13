\name{excessReturn}
\alias{excessReturn}

\title{A function that estimates excess return}

\description{ This function estimates excess return. If the the firm return is rj and market return is rM then output will be rj less rM.
}

\usage{excessReturn(firm.name, market.name, data.object)
}

\arguments{
  \item{data.object}{This is a time series object with firm return and market return}
  \item{firm.name}{It is the firm column name in the data object}
  \item{market.name}{It is the market (index) column name in the data object}

}
\value{ Excess market return}

\author{Vikram Bahure}

\examples{ 
data(marketmodelData)
er.result <- excessReturn(firm.name="ranbaxyacp",market.name="nifty",
                          data.object=mmData)

}

\keyword{excessReturn}