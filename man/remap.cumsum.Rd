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
  \item{z}{a \pkg{zoo} object indexed by event time, typically by the
    \dQuote{z.e} component obtained from \dQuote{phys2eventtime}
    function.}

  \item{is.pc}{\sQuote{logical}, whether input is a percentage. Default
  value set to \sQuote{FALSE}.}

  \item{base}{an integer specifying the base for cumulative sum.}
}

\details{This function remaps a time series into ints cumulative summation.
	 Function assigns first value as zero in the event window (-width) before cumulating the values.     
	    
}

\value{A \pkg{zoo} object with the cumulative summation for each series.
}

\seealso{
  \code{\link{phys2eventtime}}
}

\author{Chirag Anand, Vikram Bahure, Vimal Balasubramaniam}

\examples{
data(StockPriceReturns)
data(SplitDates)

es.results <- phys2eventtime(z = StockPriceReturns,
                             events = SplitDates,
                             width = 5)
es.w <- window(es.results$z.e, start = -5, end = +5)
eventtime <- remap.cumsum(es.w, is.pc = FALSE, base = 0)

print(eventtime[as.character(-3:3), ])
}
