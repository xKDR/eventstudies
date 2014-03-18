\name{remap.event.reindex}
\alias{remap.event.reindex}

\title{This function remaps the event date to 100, and other values
  relative to the remapped event date.}

%% FIXME: needs to be rewritten.
\description{A function which consumes a zoo object where there are lots
  of events (as columns). The contents are all levels.  For each column,
  the event date value is set to 100 and all other values are scaled
  accordingly.  It's input is a zoo object which is the first component of
  the list returned by phys2eventtime.
}

\usage{remap.event.reindex(z)}

\arguments{  
    \item{z}{z is a zoo object containing input data from phys2eventtime.}
}

\value{The function returns a zoo object which has been rescaled.}

\seealso{
phys2eventtime
}
\examples{
data(StockPriceReturns)
data(SplitDates)
es.results <- phys2eventtime(z=StockPriceReturns, events=SplitDates,width=5)
es.w <- window(es.results$z.e, start=-5, end=+5)
eventtime <- remap.event.reindex(es.w)
}
