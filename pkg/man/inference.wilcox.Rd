\name{inference.wilcox}
\alias{inference.wilcox}

\title{
  Wilcox inference for the event study.
}

\description{
This function does wilcox inference to generate distribution of average of all the cumulative returns time-series.
 }

\usage{
inference.wilcox(es.w,
                to.plot = TRUE,
                xlab = "Event time", 
                ylab = "Cumulative returns of response series", 
	        main = "Event study plot")
}

\arguments{
 
  \item{es.w}{a \pkg{zoo} series indexed by event time. It is the
    \dQuote{z.e} component of the list returned by function
    \dQuote{phys2eventtime}.}

  \item{to.plot}{a \sQuote{logical}. Whether to  generate an eventstudy plot of the
    inference estimated. Default is \sQuote{TRUE}.}

  \item{xlab}{If to.plot is \sQuote{TRUE}, then the plot generated will take this X label.}

  \item{ylab}{If to.plot is \sQuote{TRUE}, then the plot generated will
  take this Y label.}

  \item{main}{If to.plot is \sQuote{TRUE}, then the plot generated will
    take this as the main title.}
}

\value{A \sQuote{matrix} with 3 columns: the lower confidence interval (CI),
  the mean, and the upper CI which are the result of wilcox inference.}

\author{Vikram Bahure}

\seealso{
  phys2eventtime
}

\examples{
data(StockPriceReturns)
data(SplitDates)
es.results <- phys2eventtime(z = StockPriceReturns,
                             events = SplitDates,
                             width = 5)
es.w <- window(es.results$z.e, start = -5, end = +5)
eventtime <- remap.cumsum(es.w, is.pc = FALSE, base = 0)
infr <- inference.wilcox(es.w = eventtime, to.plot = FALSE)
head(infr)
}
