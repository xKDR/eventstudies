\name{ees}
\alias{ees}

\title{
Extreme events study for a time series.
}

\description{
This function identifies the extreme events from a time
series. It also generates summary statistics for clustered and
unclustered extreme event distribution.
 }

\usage{
ees(input,prob.value)
}

\arguments{
  \item{input}{'input' is the time-series on which extreme event
  analysis is done. This series should in returns format.}
  \item{prob.value}{It is the tail value on the basis of which the
  extreme event are defined. For eg: prob.value of 5 will consider 5\% tail on both sides.} 
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
\references{To convert number to words, code uses function \href{http://finzi.psych.upenn.edu/R/Rhelp02a/archive/46843.html}{numbers2words} by \href{http://socserv.mcmaster.ca/jfox/}{John Fox} and deprintize function by \href{http://mbq.me/}{Miron Kursa} }

\author{Vikram Bahure}
\examples{
library(eventstudies)
data(eesData)
input <- eesData$sp500
output <- ees(input, prob.value=5)
}
