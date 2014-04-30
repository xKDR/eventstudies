\name{eesPlot}
\alias{eesPlot}

\title{Plotting clustered and unclustered extreme events}

\description{ This function plots an event study with extreme events
  as identified by the function \sQuote{ees}.
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
  
  \item{z}{a time series object with the response and event series.}

  \item{response.series.name}{Column name of the series in \sQuote{z}
    that will be the \sQuote{outcome} or \sQuote{response} series.}

  \item{event.series.name}{Column name of the series in \sQuote{z} that
  will be the \sQuote{event} series.}

  \item{titlestring}{Title for event study plot}

  \item{ylab}{Y-axis label of the plot}

  \item{width}{Time window before and after the event to be plotted}

  \item{prob.value}{Cut-off values in the probability distribution (in
    percentage terms) to identify extreme events.}
}

\details{
  This function draws from many different functions in the package:
  \itemize{
    \item Identify extreme events based on \sQuote{prob.value} on the
    series named \sQuote{event.series.name} in \sQuote{z}, similar to
    the function \code{ees};
    
    \item Treats clusters in the events identified as one event by
    cumulating the series named \sQuote{response.series.name} in
    \sQuote{z} for the period in the cluster;

    \item Obtains confidence intervals using \code{inference.bootstrap}
    and the presents the anaylsis in a graph. 
  }

}

\value{A plot of the response series with lower and upper tail events
  defined on the event series.}

\references{
  Ila Patnaik, Nirvikar Singh and Ajay Shah (2013).
  Foreign Investors under stress: Evidence from
  India. \emph{International Finance}, \bold{16(2)}, 213-244.
  URL
  http://onlinelibrary.wiley.com/doi/10.1111/j.1468-2362.2013.12032.x/abstract
  and also available at http://macrofinance.nipfp.org.in/releases/PatnaikShahSingh2013_Foreign_Investors.html
}

\author{Vikram Bahure, Vimal Balasubramaniam}

\examples{
library(eventstudies)
data(EESData)
## Generating event study plots (using modified event study methodology)
eesPlot(z = EESData,
        response.series.name = "nifty",
        event.series.name = "sp500",
        titlestring = "S&P500",
        ylab = "(Cum.) change in NIFTY")
}
