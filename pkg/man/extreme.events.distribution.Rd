\name{extreme.events.distribution}
\alias{extreme.events.distribution}

\title{
 Estimating extreme event distribution of clustered and unclustered data
}

\description{
  It is the wrapper over the get.event.count function for estimating
  extreme event distribution of clustered and unclustered data 
  
 }

\usage{
extreme.events.distribution(input, prob.value)
}

\arguments{
  \item{input}{Series for which cluster distribution is to be checked}
  \item{prob.value}{It is tail value for which the extreme event is to
    be defined. For eg: prob.value of 5 will consider 5\% tail on both
    sides}
}

\value{
Output will be the distribution of clustered and unclustered extreme
event of the series 
}

\seealso{
  get.event.count
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
output <- extreme.events.distribution(input,prob.value=5)

}