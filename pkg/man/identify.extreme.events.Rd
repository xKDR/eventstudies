\name{identify.extreme.events}
\alias{identify.extreme.events}

\title{
Identify extreme events for a time series.
}

\description{
This function identifies the extreme events from a time
series. It also generates summary statistics for clustered and
unclustered extreme event distribution.
 }

\usage{
identify.extreme.events(input,prob.value)
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

\seealso{
  get.clusters.formatted
  sumstat
  extreme.events.distribution
  runlength.dist
  quantile.extreme.values
  yearly.exevent.dist
}

\examples{
data(IdentifyExevent)
input <- diff(log(input.data[,"sp500"]))
output <- identify.extreme.events(input, prob.value=5)
}
