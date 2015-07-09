\name{inference.classic}
\alias{inference.classic}

\title{Classic T-inference for event study estimator}

\description{
  This function uses standard student's t-statistic to obtain the
  estimate for response of the event and the corresponding confidence
  intervals. 
}

\usage{
inference.classic(es.w,
                  to.plot = TRUE,
                  xlab = "Event time", 
		  ylab = "Cumulative returns of response series", 
		  main = "Event study plot")
}

\arguments{
  \item{es.w}{
    a \pkg{zoo} object indexed by event time: the \dQuote{z.e}
    component of the list returned by the \code{\link{phys2eventtime}}
    function. The object should consist of more than one series.
  }

  \item{to.plot}{a \sQuote{logical} indicating whether to generate an
    event study plot of the inference estimated. Defaults to
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
  (CI), the mean, and the upper CI which are the result of inference
  drawn using student's inference.
}

\seealso{
  \code{\link{boot}}
  \code{\link{phys2eventtime}}
  \code{\link{inference.wilcox}}
  \code{\link{inference.bootstrap}}
}

\author{Sargam Jain}

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
inference.classic(es.w = eventtime,
                  to.plot = FALSE)
}
