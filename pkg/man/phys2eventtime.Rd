\name{phys2eventtime}
\alias{phys2eventtime}

\title{
  Function to convert physical dates to event dates.
}

\description{ Given a zoo time-series and event dates, the function
  converts the physical dates to event dates. The event date becomes
  zero and all other dates shift accordingly.  }

\usage{
phys2eventtime(z, events, width = 10)
}

\arguments{
  \item{z}{an object of class \pkg{zoo} containing time series data for
    which the event frame is to be generated.}

  \item{events}{
    \sQuote{data.frame} containing event dates. See \sQuote{Details}.
  }

  \item{width}{an \sQuote{integer} specifying the number of days on each
  side of the event date.}

}

\details{

  \dQuote{events} object contains two columns: \dQuote{outcome.unit}
  consists of column names of the event stock, and \dQuote{event.when}
  is the respective event date. If an event date is not found, the
  function takes the previous value of date in \code{index(z)} as the
  event date.

  For a given \dQuote{width}, if there is any \sQuote{NA} in the event
  window, then the last observation is carried forward.

  Currently this function requires \dQuote{z} to have at least one
  \emph{column}. It relies on \sQuote{colnames} of z for the series
  names, and matches it with the \dQuote{events} object. One can use
  \code{drop = FALSE} with \sQuote{[} to achieve a single-column object.
}

\value{
  Returns a \sQuote{list} of two elements:

  \item{z.e}{a zoo object indexed with event time; \dQuote{NULL} if
  there are no \dQuote{success} \dQuote{outcomes}.}

  \item{outcomes}{a character vector with outcome of each event date:}

  \itemize{
    \item{success: shows the successful use of event date.}
    \item{wdatamissing: appears when width data is missing around the event.}
    \item{wrongspan: if event date falls outside the range of physical date.}
    \item{unitmissing: when the unit (firm name) is missing in the event list.}
  }
}

\examples{
data(StockPriceReturns)
data(SplitDates)
result <- phys2eventtime(z = StockPriceReturns, events = SplitDates, width = 5)
}

\keyword{ phys2eventime }

