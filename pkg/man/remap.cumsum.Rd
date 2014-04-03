\name{remap.cumsum}
\alias{remap.cumsum}

\title{Cumulative values}

\description{
  This function remaps a time series into its cumulative summation. 
}

\usage{
remap.cumsum(z, is.pc = FALSE, base = 0)
}

\arguments{
  \item{z}{a \pkg{zoo} object indexed by event time obtained from
    \dQuote{phys2eventtime}.}

  \item{is.pc}{\sQuote{logical}, whether input is a percentage. Default
  value set to \sQuote{FALSE}.}

  \item{base}{an integer specifying the base for cumulative sum.}
}

\value{A \pkg{zoo} object with the cumulative summation for each series.
}

\seealso{
  \code{\link{phys2eventtime}}
}

\examples{
data(StockPriceReturns)
data(SplitDates)

es.results <- phys2eventtime(z = StockPriceReturns,
                             events = SplitDates,
                             width = 5)
es.w <- window(es.results$z.e, start = -5, end = +5)
eventtime <- remap.cumsum(es.w, is.pc = FALSE, base = 0)
head(eventtime[,1:5])
}
