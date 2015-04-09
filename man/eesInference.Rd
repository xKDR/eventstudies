\name{eesInference}
\alias{eesInference}

\title{Extreme event study inference estimation}

\description{This function performs event study analysis on extreme event dates 
 	     (\sQuote{eesDates}) and using formatted output
 	     (\sQuote{get.clusters.formatted})
}

\usage{
   eesInference(input, event.lists, event.window, to.remap = TRUE, remap = "cumsum", 
   		inference = "TRUE", inference.strategy = "bootstrap")
}

\arguments{
  \item{input}{
    a formatted cluster object, as returned by
  \sQuote{get.clusters.formatted} function.
  }

  \item{event.lists}{
    a \sQuote{list} of normal and purged events as returned by
    \sQuote{eesDates}.
  }

  \item{event.window}{
    an \sQuote{integer} of length 1 that specifies a
    symmetric event window around the event date.
  }

  \item{to.remap}{
    \sQuote{logical}, indicating whether or not to remap
    the data in \sQuote{input}.The default setting is \sQuote{TRUE}
  }

  \item{remap}{
    \sQuote{character}, indicating the type of remap required, 
    \dQuote{cumsum}, \dQuote{cumprod}, or \dQuote{reindex}. Used when
    \sQuote{to.remap} is \sQuote{TRUE}.
  }

  \item{inference}{
    \sQuote{logical}, specifying whether to undertake statistical
    inference and compute confidence intervals. The default setting is
    \sQuote{TRUE}.
  }

  \item{inference.strategy}{a \sQuote{character} scalar specifying the
    inference strategy to be used for estimating the confidence
    interval. Presently, two methods are available: \dQuote{bootstrap}
    and \dQuote{wilcox}. The default setting is \sQuote{bootstrap}.
  }	

}

\details{ This function performs event study analysis using
  \code{eventstudy} function on the extreme event dates of normal
  (unclustered events) and purged (clustered and unclustered events)
  sets. These interesting dates are obtained from function \sQuote{eesDates}. The
  function can estimate confidence interval using different inference
  strategies as provided by \code{eventstudy()}.

  The function does not do market model adjustment but takes the
  output of \code{get.clusters.formatted} as it's input.
}

\value{ Format of event study output is a \sQuote{matrix} containing
  mean or median estimate with confidence interval; \sQuote{NULL} if
  there are no \dQuote{success} \dQuote{outcomes}. See
  \code{\link{phys2eventtime}} for more details.
  
  A \sQuote{list} with class attribute \dQuote{ees} holding the
  following four event study output elements:

  \item{good.normal}{
    an event study inference \sQuote{matrix} for right tail unclustered events, 
    termed as normal 
  }
  
  \item{bad.normal}{
    an event study inference \sQuote{matrix} for left tail unclustered events, 
    termed as normal 
  }

  \item{good.purged}{
    an event study inference \sQuote{matrix} for right tail clustered and unclustered 
    events, termed as purged	
  }
  
  \item{bad.purged}{
    an event study inference \sQuote{matrix} for left tail clustered and unclustered 
    events, termed as purged	
  }
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

\author{Vikram Bahure, Chirag Anand}

\examples{
data(OtherReturns)

formattedClusters <- get.clusters.formatted(event.series = OtherReturns[, "SP500"], 
                                           response.series = OtherReturns[, "NiftyIndex"])

event.lists <- eesDates(formattedClusters)

inference <- eesInference(input = formattedClusters,
                          event.lists = event.lists,
                          event.window = 5)
str(inference, max.level = 2)
}
