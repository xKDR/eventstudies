\name{yearly.exevent.summary}
\alias{yearly.exevent.summary}

\title{
  Estimating the yearly distribution of the extreme event for the series
}

\description{
  Estimating the yearly distribution of the extreme event for the series
 }

\usage{
yearly.exevent.summary(tmp)
}

\arguments{
  \item{tmp}{Series for which yearly extreme event distribution is to
    be estimated}
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
# Formatting clusters
tmp <- get.clusters.formatted(event.series=input,
response.series=input, probvalue=5, event.value="nonreturns",
response.value="nonreturns")
# Yearly distribution of series
output <- yearly.exevent.summary(tmp)

}