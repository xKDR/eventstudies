\name{ees}
\alias{ees}

\title{
Extreme events study analysis: Summary statistics
}

\description{ This function generates summary statistics table from Patnaik, Shah and Singh (2013), using a univariate time series.}

\usage{
ees(input, prob.value)
}

\arguments{
 
 \item{input}{a univariate \pkg{zoo} or \pkg{xts} time series for which
   tail events identification is required.}
 
 \item{prob.value}{The cut off (in per cent) on the probability
   distribution for the tail event.}
}

\details{ Patnaik, Shah and Singh (2013) uses modified event study methodology in the paper, which is used to understand the impact of extreme tail events on a given variable (foreign investment, stock market returns in this case). Event dates are defined as those on which extreme values of returns or flows are observed. The argument \sQuote{prob.value} defines the extreme tail values on both side of the distribution as event dates.  For instance, if \sQuote{prob.value} is 2.5 then it will consider 2.5 percent tail values on both side of the empirical distribution of the variable as event dates. The paper further differentiates between clustered events (consecutive extreme events) and unclustered events which is described in detail in summary tables.
This function helps to understand the various feature of the event dates by providing following summary statistics:
   \itemize{
    \item \dQuote{Distribution of extreme events}:  Number of events in clustered, unclustered and events used and discarded from the analysis
    \item \dQuote{Run length distribution}: Table shows number of clustered events with a particular run length
    \item \dQuote{Quantile values}: Quantile wise distribution of extreme events
    \item \dQuote{Yearly distribution}: Year wise distribution of extreme events
  }
  Please refer to Patnaik, Shah and Singh (2013) for more detailed explanation.
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
data(EESData)
input <- EESData$sp500
output <- ees(input, prob.value = 5)
}
