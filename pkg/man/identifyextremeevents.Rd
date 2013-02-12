\name{identifyextremeevents}
\alias{identifyextremeevents}

\title{
Identify extreme events for a time series.
}

\description{
This function identifies the extreme events from a time
series. It also generates summary statistics for clustered and
unclustered extreme event distribution.
 }

\usage{
identifyextremeevents(input,prob.value)
}

\arguments{
  \item{input}{'input' is the time-series on which extreme event
  analysis is done. This series should in returns format.}
  \item{prob.value}{This is tail value for which the extreme event is to
  be defined. For eg: prob.value of 5 will consider 5\% tail on both sides.}
}

\value{
  Output is in a list format with following items:
  \item{Summary statistics}{Summary of the data set}
  \item{Lower tail}{Distribution of extreme events; Run length
  distribution; Quantile values; Yearly distribution; Extreme event
  data}
  \item{Upper tail}{Distribution of extreme events; Run length
  distribution; Quantile values; Yearly distribution; Extreme event
  data}
}

\examples{
data(sp500)
input <- diff(log(sp500))
output <- identifyextremeevents(input, prob.value=5)
}
