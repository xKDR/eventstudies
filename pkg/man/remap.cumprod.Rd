\name{remap.cumprod}
\alias{remap.cumprod}

\title{Cumulative geometric values}

\description{This function computes the cumulative geometric values for a given
  \pkg{zoo} object.}

\usage{
   remap.cumprod(z, is.pc = FALSE, is.returns = TRUE, base = 100)
}

\arguments{
  \item{z}{a \pkg{zoo} object indexed by event time obtained from
  \code{\link{phys2eventtime}}.} 

  \item{is.pc}{\sQuote{logical}, whether input is a percentage. Default
  value set to \sQuote{FALSE}.}

  \item{is.returns}{\sQuote{logical}, whether input is a returns series.}

  \item{base}{an integer specifying the base for cumulative product.}
}

\value{A \pkg{zoo} object with the cumulative product for each series,
  representing a buy-hold return estimate.
}


\seealso{
 \code{\link{phys2eventtime}}
}

\examples{
data(StockPriceReturns)
data(SplitDates)

## Converting to event time frame
es.results <- phys2eventtime(z = StockPriceReturns,
                             events = SplitDates,
                             width = 5)
es.w <- window(es.results$z.e, start = -5, end = +5)

## Cumulating (geometric product) event window output
eventtime <- remap.cumprod(es.w,
                           is.pc = TRUE,
                           is.returns = TRUE,
                           base = 100)

## Comparing abnormal returns (AR) and cumulative (geometric) abnormal returns (CAR)
check.output <- cbind(es.w[,1], eventtime[,1])
colnames(check.output) <- c("abnormal.returns", "cumulative.abnormal.returns")
check.output
head(eventtime[,1:5])
}

