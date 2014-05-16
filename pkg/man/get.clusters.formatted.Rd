\name{get.clusters.formatted}
\alias{get.clusters.formatted}

\title{Get formatted clusters to perform extreme event study analysis (ees)}
\description{The functions formats extreme event dates, dealing with
  clusters in the event frame.}

\usage{
get.clusters.formatted(event.series,
                       response.series,
                       probvalue = 5,
                       event.value = "nonreturns",
                       response.value = "nonreturns")
}

\arguments{
  \item{event.series}{
  	a \pkg{zoo} matrix of \sQuote{event} series.
  }
  \item{response.series}{
	a \pkg{zoo} matrix of \sQuote{response} series.
  }
  \item{probvalue}{
	The value (in percent) on the probability
    	distribution to define a tail event.
  }
  \item{event.value}{
	a \sQuote{character} scalar specifying if the \sQuote{event.series} is to be 
	converted to \sQuote{returns} or left as \sQuote{nonreturns}
  }
  \item{response.value}{
	a \sQuote{character} scalar specifying the if the \sQuote{response.series} is to be 
	converted to \sQuote{returns} or left as \sQuote{nonreturns}
  }
}

\details{
  Tail (Rare) events are often the object of interest in finance. These
  events are defined as those that have a low probability of
  occurrence. This function identifies such events based on
  \sQuote{probvalue} mentioned by the user and generates summary
  statistics about the events. If \sQuote{probvalue} is 2.5\%, events
  below 2.5\% (lower tail) and above 97.5\% (upper tail) of the
  distribution are identified as extreme events. 

  Once the extreme events are defined, this function further formats the
  events. The extreme event functionality is muddled if we have another
  event occurring in the event time frame. Following the methodology of
  Patnaik. Shah and Singh (2013), we handle clustered events. Clustered
  events are handled in following ways:

  \itemize{
    \item Clustered events which are defined as consecutive events, are fused 
    into a single event and respective returns of response series are also fused.
    \item Mixed clusters are the left and right tail events occurring on 
    consecutive days. These are identified and discarded from the analysis.
  }
}

\value{
  A \pkg{zoo} object is returned with formatted \sQuote{event.series}
  and \sQuote{response.series}. It also has separate columns to identify
  tail events, named \sQuote{left.tail} and \sQuote{right.tail}, with
  binary outcome (1 equals tail event). Finally, the object has column
  named \sQuote{cluster.pattern} which identifies the length of the
  cluster in the event series.
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
data(OtherReturns)

gcf <- get.clusters.formatted(event.series = OtherReturns$SP500, 
       			      response.series = OtherReturns$NiftyIndex)

str(gcf, max.level = 2)
}
