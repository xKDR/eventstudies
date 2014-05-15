\name{eesInference}
\alias{eesInference}

\title{Extreme event study inference estimation}

\description{This function performs event study analysis on extreme event dates 
 	     (\sQuote{eesDates}) and formatted output (\sQuote{get.clusters.formatted})
}

\usage{
   eesInference(input, eventLists, width, to.remap = TRUE, remap = "cumsum", 
   		inference = "TRUE", inference.strategy = "bootstrap")
}

\arguments{
	\item{input}{
	an output object of \sQuote{get.clusters.formatted}
	}

	\item{eventLists}{
	an output object of \sQuote{eesDates}, which provides event list 
	for normal and purged events
	}

	\item{width}{
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

\details{
	This function performs event study analysis on the extreme event dates of normal 
	(unclustered events) and purged (clustered and unclustered events) sets. These 
	dates are obtained from function \sQuote{eesDates}. The function also estimates 
	confidence interval using different inference strategies 
	(\sQuote{bootstrap,wilcoxon}). The functionalities are similar to 
	\sQuote{eventstudy} function without market model adjustment and \sQuote{input} 
	is output of \sQuote{get.clusters.formatted}, not \sQuote{firm.returns}.
}

\value{
	Format of event study output is a \sQuote{matrix} containing mean (bootstrap) 
	or median (with wilcoxon) estimate with confidence interval; \sQuote{NULL} if there
  	are no \dQuote{success} \dQuote{outcomes}
	
	A list with class attribute \dQuote{ees} holding the
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
## Formatting extreme event dates
input <- get.clusters.formatted(event.series = OtherReturns[,"SP500"], 
      	                        response.series = OtherReturns[,"NiftyIndex"])	

## Extracting event dates
event.lists <- eesDates(input)

## Performing event study analysis and computing inference
inf <- eesInference(input = input, eventLists = event.lists, width = 5)
str(inf, max.level = 2)
}
