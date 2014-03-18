\name{remap.cumprod}
\alias{remap.cumprod}

\title{Compute geometric returns.}

\description{This function computes the geometric returns for a given
  \pkg{zoo} object.}

\usage{remap.cumprod(z, is.pc = TRUE, is.returns = TRUE, base = 100)}

\arguments{

  \item{z}{a \pkg{zoo} object indexed by event time, as returned from
    \dQuote{phys2eventtime}.}

  \item{is.pc}{\sQuote{logical}, whether input is a percentage.}

  \item{is.returns}{\sQuote{logical}, whether input is returns.}

  \item{base}{an integer specifying the base for cumulative product.}
}

\value{A \pkg{zoo} series is returned with each column replaced by the
  product of \dQuote{base} and the cumulative product of the column.}

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

%% FIXME: is base=0 good an example?
eventtime <- remap.cumprod(es.w,
                           is.pc = FALSE,
                           is.returns = TRUE,
                           base = 0)
}

