\name{sumstat}
\alias{sumstat}

\title{
 Estimating summary for the data
}

\description{
  Estimating summary for the data
 }

\usage{
sumstat(input)
}

\arguments{
  \item{input}{Series for which summary statistics is to be generated}
}

\value{
Output will generate minimum, 5\%, 25\%, median, mean, 75\%, 95\%,
maximum, standard deviation, IQR and number of observations
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
output <- sumstat(input)

}