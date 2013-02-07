\name{yearly.exevent.dist}
\alias{yearly.exevent.dist}

\title{
 Estimating the yearly distribution of the extreme event for the series
}

\description{
  It is the wrapper over the yearly.exevent.summary function to get the
  yearly distribution of the extreme event series
 }

\usage{
yearly.exevent.dist(input, prob.value)
}

\arguments{
  \item{input}{Series for which yearly extreme event distribution is to
    be estimated}
  \item{prob.value}{It is tail value for which the extreme event is to
    be defined. For eg: prob.value of 5 will consider 5\% tail on both
    sides}
}

\value{
Output will generate yearly distribution of extreme events
}

\seealso{
  get.clusters.formatted
  yearly.exevent.summary
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
output <- yearly.exevent.dist(input,prob.value=5)

}