\name{phys2eventtime}
\alias{phys2eventtime}

\title{
  Function to convert physical dates to event dates.
}

\description{ Given a zoo time-series and event dates, it converts the
  physical dates to event dates. The event date becomes zero and all
  other dates shift accordingly.  }

\usage{
phys2eventtime(z, events, width = 10)
}

\arguments{
  \item{z}{an object of class \pkg{zoo} containing time series data for
    which the event frame is to be generated. See \sQuote{Details}.}

  %% FIXME: should remove this description from eventstudy.Rd, and point
  %% it here? eventstudy is a general function, it will need all args description?
  \item{events}{
    \sQuote{data.frame} of two columns containing event
    dates. \dQuote{outcome.unit} consists of column names of the event
    stock, and \dQuote{event.when} is the respective event date. See
    \sQuote{Details}.
  }

  \item{width}{an \sQuote{integer} specifying the number of days on each
  side of the event date. For a given width, if there is any \sQuote{NA}
  in the event window, then the last observation is carried forward.}

}

\details{
  The function is used to convert physical dates to event
  dates. If an event date is not found, the function takes the previous
  value of date in \code{index(z)} as the event date.

  Currently, this function requires \dQuote{z} to have at least one
  \emph{column}. It relies on colnames of the object for the series
  names, and matches it with the \dQuote{events} object. One can use
  \code{drop = FALSE} with \sQuote{[} to achieve a single-column object.
}

\value{
  Returns a \sQuote{list} of two elements:
  \item{z.e}{a zoo object indexed with event time.}

  \item{outcomes}{
    a \sQuote{character} vector having outcome of each event
    date. Provides four outcomes: \cr
    \dQuote{success}: shows the successful use of event date. \cr
    \dQuote{wdatamissing}: appears when width data is missing around
    the event. \cr
    \dQuote{wrongspan}: if event date falls outside the range of physical
    date. \cr
    \dQuote{unimissing}: when the unit (firm name) is missing in the event
    list.
    }
}

\examples{
data(StockPriceReturns)
data(SplitDates)
phys2eventtime(z = StockPriceReturns, events = SplitDates,width = 5)
}

\keyword{ phys2eventime }

