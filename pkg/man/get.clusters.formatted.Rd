\name{get.clusters.formatted}
\alias{get.clusters.formatted}

\title{
 Formatting the clusters in the extreme event in the data-set
}

\description{
 The function marks the clusters in the event series, then
 clusters the response series for the clustered dates of the event
 series by summing the returns of the response series. It removes the
 mixed clusters in the process and finally gives the output in a
 time-series format.
 }

\usage{
get.clusters.formatted(event.series, response.series, probvalue,
event.value, response.value)
}

\arguments{
  \item{event.series}{Series on which event is to be defined}
  \item{response.series}{Series on which response is to generated}
  \item{probvalue}{It is tail value for which the extreme event is to
    be defined. For eg: prob.value of 5 will consider 5\% tail on both
    sides}
  \item{event.value}{If the event series is in returns then
  event.value="nonreturns" else event.value="returns"}
  \item{response.value}{If the response series is in returns then
  response.value="nonreturns" else response.value="returns"}
}

\value{
Output is the time-series object with formatted clusters.
}

\seealso{
  gen.data
  summarise.cluster
  exact.pattern.location
  identify.mixedclusters
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
# Formatting clusters
#If there is no response series then feed event.series as
# response.series 
output <- get.clusters.formatted(event.series=input,
response.series=input, probvalue=5, event.value="nonreturns",
response.value="nonreturns")
}