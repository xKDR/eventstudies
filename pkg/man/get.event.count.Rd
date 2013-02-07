\name{get.event.count}
\alias{get.event.count}

\title{
 Estimating extreme event distribution of clustered and unclustered data
}

\description{
  Estimating extreme event distribution of clustered and unclustered data  
 }

\usage{
get.event.count(series, probvalue, value)
}

\arguments{
  \item{series}{Series for which cluster distribution is to be checked}
  \item{probvalue}{It is tail value for which the extreme event is to
    be defined. For eg: prob.value of 5 will consider 5\% tail on both
    sides}
  \item{value}{If the input is in returns then value="nonreturns" else
  value="returns"}
}

\value{
  Output will be the distribution of clustered and unclustered extreme
  event of the series 
}

\seealso{
  gen.data
  get.clusters.formatted
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
output <- get.event.count(series=input,probvalue=5, value="nonreturns")

}