\name{ees}
\alias{ees}

\title{ Summary statistics of extreme events }

\description{
  This function generates summary statistics for identification and
  analysis of extreme events. 
}

\usage{
   ees(input, prob.value)
}

\arguments{
 
  \item{input}{a univariate \pkg{zoo} or \pkg{xts} time series object}
  
  \item{prob.value}{The value (in percent) on the probability
    distribution to define a tail event.}
}

\details{
  Tail (Rare) events are often the object of interest in finance. These
  events are defined as those that have a low probability of
  occurrence. This function identifies such events based on
  \sQuote{prob.value} mentioned by the user and generates summary
  statistics about the events. If \sQuote{prob.value} is 2.5\%, events
  below 2.5\% (lower tail) and above 97.5\% (upper tail) of the
  distribution are identified as extreme events. 

  Following statistics is generated for both lower and upper tail
  events: 

  \itemize{
    \item \sQuote{extreme.event.distribution} provides summary
    statistics on the number of consecutive events (\dQuote{clustered}
    events) and those that are not (\dQuote{unclustered}
    events). Consecutive events that are \dQuote{mixed}, i.e., with
    upper (lower) tail event occurring after a lower (upper) tail event,
    are classified separately. 

    \item \sQuote{runlength}: When events are \dQuote{clustered},
    \sQuote{runlength} classifies such clusters into different duration
    bins.  

    \item \sQuote{quantile.values}: Within such events,
    \sQuote{quantile.values} provide the probability distribution values
    at 0\%, 25\%, 50\%, 75\% and 100\%, alongside the mean.
    
    \item \sQuote{yearly.extreme.event}: A year-wise tabulation of such
    extreme events, with a clustered event taken as one event. 
  }  
}

\value{ A \code{list} object containing:

  \item{data.summary}{a \sQuote{data.frame} containing summary of
    the data set minimum, maximum, inter-quartile range, mean, median,
    standard deviation and quantile values at 5\%, 25\%, 75\% and 95\%.}

  \item{lower.tail}{a \sQuote{list} that contains
    \sQuote{extreme.event.distribution}, \sQuote{runlength},
    \sQuote{quantile.values} and \sQuote{yearly.extreme.event} for the
    events on the lower tail of the distribution. See
    \sQuote{Details}.}

    \item{upper.tail}{a \sQuote{list} that contains
    \sQuote{extreme.event.distribution}, \sQuote{runlength},
    \sQuote{quantile.values} and \sQuote{yearly.extreme.event} for the
    events on the upper tail of the distribution. See
    \sQuote{Details}.}
}

\references{
  \cite{Ila Patnaik, Nirvikar Singh and Ajay Shah (2013).
    Foreign Investors under stress: Evidence from
    India.
    International Finance, 16(2), 213-244.
    \url{http://onlinelibrary.wiley.com/doi/10.1111/j.1468-2362.2013.12032.x/abstract}
    \url{http://macrofinance.nipfp.org.in/releases/PatnaikShahSingh2013_Foreign_Investors.html}
  }
}

\author{Vikram Bahure, Vimal Balasubramaniam}

\examples{
data(EESData)
r <- ees(EESData$sp500, prob.value = 5)
str(r, max.level = 2)
}
