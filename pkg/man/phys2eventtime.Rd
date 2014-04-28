\name{phys2eventtime}
\alias{phys2eventtime}

\title{
  Convert data from physical to event time
}

\description{\sQuote{phys2eventtime} is used to convert data from
  physical to event time using information on events identified by the
  user.}

\usage{
phys2eventtime(z, events, width = 10)
}

\arguments{
  \item{z}{an object of class \pkg{zoo} or \pkg{xts} containing data to
    be converted into event time.}

  \item{events}{
    \sQuote{data.frame} containing event identifiers. See \sQuote{Details}.
  }

  \item{width}{an \sQuote{integer} specifying the event window within
    which data should be available to consider the outcome a 'success'. See
  \sQuote{Details}.}

}

\details{

  \dQuote{events} object contains two columns: \dQuote{outcome.unit}
  consists of names of the event, and \dQuote{event.when} is the
  respective event identifier. For instance, if \sQuote{z} is a matrix
  of class \pkg{xts} with 10 stocks over 300 days, the names of stocks
  in \sQuote{z} is the superset of names for the event and the time
  period (i.e., 300 days) will be the superset for defining event dates.
  If an event date does not lie within this period, the function
  approximates to the nearest previous date using
  \code{\link{findInterval}}.

  The argument \dQuote{width} provides the user with an option to define
  successful events as those that have data within a window around the
  event. If \dQuote{width} is 10 periods, those events with \sQuote{NA}
  within 10 periods before and after the event will be classified as
  \dQuote{wdatamissing}, otherwise, the event \dQuote{outcome} will be
  classified as \dQuote{success}.

  Currently this function requires \dQuote{z} to have at least one
  \emph{column}. It relies on \sQuote{colnames} of \dQuote{z} for the series
  names, and matches it with the \dQuote{events} object. One can use
  \code{drop = FALSE} with \sQuote{[} to achieve a single-column object.
}

\value{
  Returns a \sQuote{list} of two elements:

  \item{z.e}{a \pkg{zoo} object indexed with event time; \dQuote{NULL} if
  there are no \dQuote{success} in \dQuote{outcomes}.}

  \item{outcomes}{a character vector with outcome definition for each event.}
  \itemize{
    \item{success: the successful use of an event.}
    \item{wdatamissing: when the \dQuote{width} condition is not satisfied.}
    \item{wrongspan: when event date cannot be mapped to the physical
      time in \dQuote{z}.}
    \item{unitmissing: when the unit (a column) is missing in \dQuote{z}.}
  }
}

\seealso{
  \code{\link{findInterval}}
}

\examples{
data(StockPriceReturns)
data(SplitDates)

## Converting physical dates to event time
result <- phys2eventtime(z = StockPriceReturns,
			 events = SplitDates,
			 width = 5)

## Checking conversion to event time frame for first successful event date
c.no <- as.numeric(colnames(result$z.e))
cnames <- SplitDates[c.no[1],]
phys.output <- as.numeric(result$z.e[as.character(c(-5:5)), as.character(c.no[1])])
loc <- which(index(StockPriceReturns) \%in\% SplitDates[c.no[1], "event.when"])
raw.data <- as.numeric(StockPriceReturns[c((loc-5):(loc+5)),
                       SplitDates[c.no[1], "outcome.unit"]])
check.output <- cbind(raw.data, phys.output)
check.output
}
\keyword{phys2eventime}
