\name{ees}
\alias{ees}

\title{
Extreme events study analysis for a univariate time series
}

\description{
This function identifies tail events on a univariate time series, generates summary statistics for clustered and unclustered tail events.}

\usage{
ees(input,prob.value)
}

\arguments{
  \item{input}{A univariate time series for which tail events identification is required.}
  \item{prob.value}{The cut off (in percent) on the probability distribution for the tail event.}
}

\value{ A \code{list} object that contains: 
  \item{Summary statistics}{Summary of the data set}
 \item{Lower tail}{Distribution of extreme events; Run length
  distribution; Quantile values; Yearly distribution; and the tail events data}
  \item{Upper tail}{Distribution of extreme events; Run length
  distribution; Quantile values; Yearly distribution; and the tail events  data}
}

\references{To convert number to words, code uses function \href{http://finzi.psych.upenn.edu/R/Rhelp02a/archive/46843.html}{numbers2words} by \href{http://socserv.mcmaster.ca/jfox/}{John Fox} and deprintize function by \href{http://mbq.me/}{Miron Kursa} }

\author{Vikram Bahure}
\examples{
library(eventstudies)
data(eesData)
input <- eesData$sp500
output <- ees(input, prob.value=5)
}
