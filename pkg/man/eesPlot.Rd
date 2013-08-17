\name{eesPlot}
\alias{eesPlot}

\title{
Plotting clustered and unclustered extreme events retuns.
}

\description{
This function creates an event study plot that treats all clustered events as one event. It plots both the lower and upper tail events and the events are defined based on the cut off probability values provided (in percentage terms).}


\usage{
eesPlot(z, response.series.name, event.series.name, titlestring, ylab, width, prob.value)
}

\arguments{
  \item{z}{Data object with both response and event series on which event study is to be performed}
  \item{response.series.name}{Column name of the series in 'z' on which response is to be observed }
  \item{event.series.name}{Column name of the series in 'z' on which event is to be defined }
  \item{titlestring}{Title for event study plot}
  \item{ylab}{Y-axis label}
  \item{width}{Width for event study plot}
  \item{prob.value}{Cut-off values in the probability distribution (in percentage terms)} 
}

\value{
A plot of the response series with lower and upper tail events defined on the event series.}

\examples{
library(eventstudies)
data(eesData)
eesPlot(z=eesData, response.series.name="nifty", event.series.name="sp500",titlestring="S&P500", ylab="(Cum.) change in NIFTY")
}