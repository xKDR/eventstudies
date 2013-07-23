\name{inference.bootstrap}
\alias{inference.bootstrap}

\title{
Bootstrap inference for the event study.
}

\description{
This function does bootstrap inference to generate distribution of average of all the cumulative returns time-series.
 }

\usage{
inference.bootstrap(z.e, to.plot = TRUE, xlab = "Event time", 
			 ylab = "Cumulative returns of response series", 
			 main = "Event study plot")
}

\arguments{
  \item{z.e}{z.e is the first component of the list returned by the function phys2eventtime.}
  \item{to.plot}{This argument will generate an eventstudy plot of the inference estimated. If to.plot is equal to TRUE then function would generate the plot else it would not. }
  \item{xlab}{If to.plot is TRUE then the plot generated will take this X label}
  \item{ylab}{If to.plot is TRUE then the plot generated will take this Y label}	
  \item{main}{If to.plot is TRUE then the plot generated will take this as main title}	
}

\value{
A data frame with 3 columns, the lower confidence interval (CI), the mean and the upper CI which are the result of bootstrap inference.
}

\seealso{
phys2eventtime
}

\examples{
data(StockPriceReturns)
data(SplitDates)
es.results <- phys2eventtime(z=StockPriceReturns, events=SplitDates,width=5)
es.w <- window(es.results$z.e, start=-5, end=+5)
eventtime <- remap.cumsum(es.w, is.pc=FALSE, base=0)
inference.bootstrap(z.e=eventtime, to.plot=FALSE)
}