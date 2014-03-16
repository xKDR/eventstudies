\name{ees}
\alias{ees}

\title{
Extreme events study analysis for a univariate time series
}

\description{This function identifies tail events on a univariate time
  series, generates summary statistics for clustered and unclustered
  tail events.}

\usage{
ees(input, prob.value)
}

\arguments{
 
 \item{input}{a univariate \pkg{zoo} time series for which tail events
   identification is required.}
 
 \item{prob.value}{The cut off (in percent) on the probability
   distribution for the tail event.}
}

\value{ A \code{list} object containing:

  \item{data.summary}{a \sQuote{data.frame} containing summary of
    the data set.}

  \item{lower.tail}{a \sQuote{list} containing: distribution of extreme
    events, run length distribution, quantile values, yearly
    distribution, and the tail events data.}

  \item{upper.tail}{a \sQuote{list} containing: distribution of extreme
    events, run length distribution, quantile values, yearly
    distribution, and the tail events data.}
}

\references{To convert number to words, code uses function
  \href{http://finzi.psych.upenn.edu/R/Rhelp02a/archive/46843.html}{\dQuote{numbers2words}}
  by \href{http://socserv.mcmaster.ca/jfox/}{John Fox} and
  \dQuote{deprintize} function by \href{http://mbq.me/}{Miron Kursa}.
}

\author{Vikram Bahure}
\examples{
library(eventstudies)
data(eesData)
input <- eesData$sp500
output <- ees(input, prob.value = 5)
}
