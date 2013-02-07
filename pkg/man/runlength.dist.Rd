\name{runlength.dist}
\alias{runlength.dist}

\title{
 Estimating runlength distribution for the clusters
}

\description{
 Estimating runlength distribution of the clusters in the extreme
 event. It is wrapper over get.cluster.distribution 
 }

\usage{
runlength.dist(input, prob.value)
}

\arguments{
  \item{input}{Series for which quantile values for tail is to be estimated}
  \item{prob.value}{It is tail value for which the extreme event is to
    be defined. For eg: prob.value of 5 will consider 5\% tail on both
    sides}
}

\value{
  Output is the runlength distribution for the clusters
}

\seealso{
  get.clusters.formatted
  get.cluster.distribution
  numbers2words
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
output <- runlength.dist(input, prob.value=5)
}