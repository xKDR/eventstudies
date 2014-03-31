\name{inference.bootstrap}
\alias{inference.bootstrap}

\title{Do bootstrap inference for event study analysis}

\description{
  This function does bootstrap inference to generate distribution of
  average of all the cumulative returns time-series.
}

\usage{
inference.bootstrap(es.w,
                   to.plot = TRUE,
                   xlab = "Event time", 
		   ylab = "Cumulative returns of response series", 
		   main = "Event study plot")
}

\arguments{
  \item{es.w}{
    a \pkg{zoo} series indexed by event time: the \dQuote{z.e}
    component of the list returned by the \code{\link{phys2eventtime}}
    function.
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

%% FIXME: more details
\details{
  Function \sQuote{boot} from package \pkg{boot} is used for
  bootstrapping with \emph{replicates} of 1000.
}

\value{
  A \sQuote{matrix} with 3 columns, the lower confidence interval
  (CI), the mean, and the upper CI which are the result of bootstrap
  inference.
}

\seealso{
  phys2eventtime
}

\examples{
data(StockPriceReturns)
data(SplitDates)

es.results <- phys2eventtime(z = StockPriceReturns,
                             events = SplitDates,
                             width = 5)
es.w <- window(es.results$z.e,
               start = -5,
               end = +5)

eventtime <- remap.cumsum(es.w, is.pc = FALSE, base = 0)

inference.bootstrap(es.w = eventtime,
                    to.plot = FALSE)
}
