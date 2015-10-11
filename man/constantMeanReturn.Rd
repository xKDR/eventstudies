\name{constantMeanReturn}
\alias{constantMeanReturn}

\title{Extract residuals over constant mean returns}

\description{
  This function computes constant mean return during the estimation
  period prior to the defined event window. If the firm return is
  \dQuote{firm.returns}, then output will be \dQuote{firm.returns}
  during the event period less the constant mean return computed over
  the estimation period. 
}

\usage{constantMeanReturn(firm.returns, residuals = TRUE)}

\arguments{
  \item{firm.returns}{a \pkg{zoo} timeseries of firm returns from
    which constant mean return is computed over the estimation period.}

  \item{residuals}{a \sQuote{logical} indicating whether to return
    residuals or \sQuote{constant mean}. When argument to the function
    includes the entire time series, returns are estimated using the
    entire data set and not just estimation period, value of residuals
    should be TRUE in such a case.}
}
\value{Residual returns unexplained by constant mean returns}

\author{Sargam Jain}

\examples{ 
data(StockPriceReturns)
data(SplitDates)
cmr.result <- constantMeanReturn(firm.returns = StockPriceReturns,
                              residuals = TRUE)

Comparison <- merge(meanAdjustedReturns = cmr.result$Infosys,
                    Infosys = StockPriceReturns$Infosys,
                    all = FALSE)
plot(Comparison)
}

\keyword{constantMeanReturn}
