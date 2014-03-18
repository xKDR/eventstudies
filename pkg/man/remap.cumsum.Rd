\name{remap.cumsum}
\alias{remap.cumsum}

\title{This function gives the cumulative total of the values (in
  level or percentage form).}

\description{
  This function gives the cumulative total of the values. 
}

\usage{
remap.cumsum(z, is.pc = TRUE, base = 0)
}

\arguments{
  \item{z}{a \pkg{zoo} object indexed by event time, as returned from
    \dQuote{phys2eventtime}.}

  \item{is.pc}{\sQuote{logical}, whether input is a percentage.}

  \item{base}{an integer specifying the base for cumulative sum.}
}

\value{A \pkg{zoo} series is returned having each column replaced by
  the cumulative sum of the column.}

\seealso{
  eventDays, eventstudyData, phys2eventtime
}

\examples{
data(StockPriceReturns)
data(SplitDates)

es.results <- phys2eventtime(z = StockPriceReturns,
                             events = SplitDates,
                             width = 5)
es.w <- window(es.results$z.e, start = -5, end = +5)
eventtime <- remap.cumsum(es.w, is.pc = FALSE, base = 0)
}
