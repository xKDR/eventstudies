\name{eesPlot}
\alias{eesPlot}

\title{
Extreme event study analysis: Plotting clustered and unclustered extreme events returns
}

\description{ This function uses modified event study methodology from Patnaik, Shah and Singh (2013) to plot the impact of event series on the response series. 
}

\usage{
eesPlot(z,
        response.series.name,
        event.series.name,
        titlestring,
        ylab,
        width,
        prob.value)
}

\arguments{
  
  \item{z}{Data object with both response and event series on which
    event study is to be performed.}

  \item{response.series.name}{Column name of the series in \sQuote{z} on which
    response is to be observed.}

  \item{event.series.name}{Column name of the series in \sQuote{z} on which
    event is to be defined.}

  \item{titlestring}{Title for event study plot.}

  \item{ylab}{Y-axis label of the plot.}

  \item{width}{Width for event study plot.}

  \item{prob.value}{Cut-off values in the probability distribution (in
    percentage terms).}
}

\details{Patnaik, Shah, and Singh (2013) use modified event study methodology to draw the event study plot. The function draws the event dates from the tail of the event series. Extreme events are then formatted by fusing the consecutive events on same side of the tail into one event, further fusing the response series on a clustered event and removing the mixed clusters (consecutive events from left and right tails). After formatting the events, response series is converted to event time frame and mean response is constructed for all event dates. The function also estimates confidence interval using bootstrap inference strategy.  

The paper defines extreme tail events as event dates and the argument \sQuote{prob.value} is used for the same. The function using  the argument \sQuote{event.series.name} picks a column from the data object \sQuote{z}, which is used to extract extreme event dates. While, argument \sQuote{response.series.name} gets the response series on which impact of the extreme event dates is to be analysed. 

Please refer to the paper Patnaik, Shah and Singh (2013) for detailed explanation.
}

\value{A plot of the response series with lower and upper tail events
  defined on the event series.}

\examples{
library(eventstudies)
data(EESData)
eesPlot(z = EESData,
        response.series.name = "nifty",
        event.series.name = "sp500",
        titlestring = "S&P500",
        ylab = "(Cum.) change in NIFTY")
}
