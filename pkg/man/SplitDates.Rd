\name{SplitDates}

\docType{data}

\alias{SplitDates}

\title{A set of events to perform eventstudy analysis.}

\description{ This data set contains stock split event dates for the index
  constituents of the Bombay Stock Exchange index (SENSEX). The data
  format follows the required format in the function
  \code{phys2eventtime}, with two columns 'outcome.unit' (firm name) and
  'event.when' (stock split date).}

\usage{data(SplitDates)}

\format{\code{data.frame}}

\examples{
    data(SplitDates)
}
\keyword{datasets}
