\name{AMMData}
\alias{AMMData}


\title{Data containing firm returns, market returns, and currency
  returns used for AMM estimation}

\description{The data series is a daily time-series zoo object. The sample range for the data is from 2012-02-01 to 2014-01-31. It consists daily time series for firm returns (Infosys and TCS), market returns (Nifty returns) and currency returns (INR/USD). This data is used to demonstrate the AMM estimation.}

\usage{data(AMMData)}

\author{Vikram Bahure}

\examples{
library(zoo)
data(AMMData)
str(AMMData)
}

\keyword{AMMData}
