\name{remap.event.reindex}
\alias{remap.event.reindex}

\title{
  Reindex value within event window
}

\description{
  Reset value at the beginning of the event window to a 100 and reindex thereon.
}

\usage{remap.event.reindex(z)}

\arguments{
  \item{z}{z is a zoo object obtained from
    \code{\link{phys2eventtime}}. 
  }
}

\value{Rescaled returns value}

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
eventtime <- remap.event.reindex(es.w)
head(eventtime[,1:5])
}
