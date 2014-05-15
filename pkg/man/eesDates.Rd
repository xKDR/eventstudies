\name{eesDates}
\alias{eesDates}

\title{Event list for extreme event study analysis }

\description{
  This function creates event list (clustered and unclustered events) for extreme event study analysis.
}

\usage{
   eesDates(input)
}

\arguments{
	\item{input}{an output object of \sQuote{get.clusters.formatted}}
}

\details{
	This function creates event list for extreme event study analysis. The event list 
	consists of extreme right tail and left tail event dates for clustered and 
	unclustered data. The \sQuote{normal} set consists of event dates for only 
	unclustered events and \sQuote{purged} set consists of event dates for unclustered 
	and clustered both. Unclustered events consists of clean event window with no event 
	occurring in the event window and clustered events are fused consecutive events 
	which lie in the same tail.
}

\value{ 
	A \code{list} object containing:
	\item{events.good.normal}{\sQuote{data.frame} containing events list as an input 
	to \sQuote{eventstudy} function, containing right tail event dates of unclustered
	events only}
	\item{events.bad.normal}{\sQuote{data.frame} containing events list as an input 
	to \sQuote{eventstudy} function, containing left tail event dates of unclustered 
	events only}
	\item{events.good.purged}{\sQuote{data.frame} containing events list as an input 
	to \sQuote{eventstudy} function, containing right tail event dates of unclustered
	events and unclustered events}
	\item{events.bad.purged}{\sQuote{data.frame} containing events list as an input 
	to \sQuote{eventstudy} function, containing left tail event dates of unclustered 
	and clustered events}
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
str(event.lists, max.level = 2)
}
