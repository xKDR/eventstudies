\name{get.cluster.distribution}
\alias{get.cluster.distribution}

\title{
 Estimating runlength distribution for the clusters
}

\description{
 Estimating runlength distribution of the clusters in the extreme
 event. 
 }

\usage{
get.cluster.distribution(tmp, variable)
}

\arguments{
  \item{tmp}{It is the output of the get.clusters.formatted}
  \item{variable}{Variable on which cluster distribution is to be
  estimated; variable="event.series"}
}

\value{
  Output is the runlength distribution for the clusters
}

\seealso{
  get.clusters.formatted
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
tmp <- get.clusters.formatted(event.series=input,response.series=input,
probvalue=5,event.value="nonreturns",response.value="nonreturns")
output <- get.cluster.distribution(tmp, variable="event.series")
}