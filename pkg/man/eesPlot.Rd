\name{eesPlot}
\alias{eesPlot}

\title{
Plotting clustered and unclustered extreme event study plot.
}

\description{
This function generates an extreme event study plot by marking extreme events and fusing clustered events together. It plots event study plot for lower tail and upper tail events. Tail events are defined as per given probability value. The value of the event series and response series should be in returns.
 }

\usage{
eesPlot(z, response.series.name, event.series.name, titlestring, ylab, width, prob.value)
}

\arguments{
  \item{z}{Data object with both response and event series on which event study is to be performed}
  \item{response.series.name}{Column name of the series in 'z' on which response is to be observed }
  \item{event.series.name}{Column name of the series in 'z' on which event is to be observed }
  \item{titlestring}{Title for event study plot: Preferred to be response series name}
  \item{ylab}{Y-axis label}
  \item{width}{Width for event study plot}
  \item{prob.value}{It is the tail value on the basis of which the
  extreme event are defined. For eg: prob.value of 5 will consider 5\% tail on both sides.} 
}

\value{
 It gives an extreme event study plot for very bad (lower tail) and very good (upper tail) events on event series. 
}

\examples{
library(eventstudies)
data(eesData)
eesPlot(z=eesData, response.series.name="nifty", event.series.name="sp500",titlestring="S&P500", ylab="(Cum.) change in NIFTY")
}
