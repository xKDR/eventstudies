\name{remap.event.reindex}
\alias{remap.event.reindex}

\title{Remaps the event date to 100 and remaps other values in event window}

%% FIXME: needs to be rewritten.
\description{This function remaps the event date to 100, and other values
  relative to the remapped event date.
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
