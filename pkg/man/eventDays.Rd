\name{eventDays}

\docType{data}

\alias{eventDays}

\title{List of event dates for S&P 500}

\description{
  This series is an input for the examples in the eventstudy framework. It is a data frame with two columns 'unit' and 'when'. The column 'when' has event dates for S&P 500 while column 'unit' has list of response series' column names. In this data frame, 'unit' is 'nifty' which corresponds with column name of the 'eventstudyData'. Here, 1% tail values are termed as extreme events days, in this example we take upper tail events.
}

\usage{data(eventDays)}

\format{An object with class attributes \code{data.frame} containing event dates for eventstudy.}

\examples{
    data(eventDays)
}
\keyword{datasets}
