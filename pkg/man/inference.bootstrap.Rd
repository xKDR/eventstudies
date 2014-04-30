\name{inference.bootstrap}
\alias{inference.bootstrap}

\title{Bootstrap inference for event study estimator}

\description{
  This function obtains a boostrapped confidence interval for estimates
  of magnitude over the event horizon. 
}

\usage{
inference.bootstrap(es.w,
                   to.plot = TRUE,
                   boot.run = 1000,
                   xlab = "Event time", 
		   ylab = "Cumulative returns of response series", 
		   main = "Event study plot")
}

\arguments{
  \item{es.w}{
    a \pkg{zoo} object indexed by event time: the \dQuote{z.e}
    component of the list returned by the \code{\link{phys2eventtime}}
    function.
  }

  \item{boot.run}{
    A \sQuote{numeric}, controlling the number of simulations required
    for the bootstrap.
  }

  \item{to.plot}{a \sQuote{logical} indicating whether to generate an
    eventstudy plot of the inference estimated. Defaults to
    \sQuote{TRUE}.
  }

  \item{xlab}{the x-axis label of the generated plot. Used if
    \dQuote{to.plot} is \sQuote{TRUE}.}
  
  \item{ylab}{the y-axis label of the generated plot. Used if
    \dQuote{to.plot} is \sQuote{TRUE}.}

  \item{main}{main title of the plot. Used if \dQuote{to.plot} is
    \sQuote{TRUE}.}
}

\value{
  A \sQuote{matrix} with 3 columns, the lower confidence interval
  (CI), the mean, and the upper CI which are the result of bootstrap
  inference.
}

\seealso{
  \code{\link{boot}}
  \code{\link{phys2eventtime}}
  \code{\link{inference.wilcox}}
}

\author{Vikram Bahure, Vimal Balasubramaniam}

\examples{
data(StockPriceReturns)
data(SplitDates)
## Converting physical dates to event time frame
es.results <- phys2eventtime(z = StockPriceReturns,
                             events = SplitDates,
                             width = 5)
es.w <- window(es.results$z.e,
               start = -5,
               end = +5)

## Cumulating event window
eventtime <- remap.cumsum(es.w, is.pc = FALSE, base = 0)

## Constructing confidence interval using bootstrap inference strategy
inference.bootstrap(es.w = eventtime,
                    to.plot = FALSE)
}
