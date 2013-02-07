\name{quantile.extreme.values}
\alias{quantile.extreme.values}

\title{
 Estimating quantile values for the tail events
}

\description{
 Estimating quantile values for the tail events
 }

\usage{
quantile.extreme.values(input, prob.value)
}

\arguments{
  \item{input}{Series for which quantile values for tail is to be estimated}
  \item{prob.value}{It is tail value for which the extreme event is to
    be defined. For eg: prob.value of 5 will consider 5\% tail on both
    sides}
}

\value{
Quantile values for tail events
}

\seealso{
  get.clusters.formatted
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
output <- quantile.extreme.values(input, prob.value=5)
}