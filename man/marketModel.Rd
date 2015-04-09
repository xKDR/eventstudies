\name{marketModel}
\alias{marketModel}

\title{Extract residuals from a market model}

\description{This function extracts residuals from a market model using
  function \code{stats:lm}. \sQuote{na.exclude} is passed as
  \sQuote{na.action} for \sQuote{lm}. For more than one firm, the
  function merges \dQuote{market.returns} with each element of
  \dQuote{firm.returns} before passing to \sQuote{lm} so that rows of
  firms without \sQuote{NA}s are not removed from the zoo object.
}

\usage{marketModel(firm.returns, market.returns, residuals = TRUE)}

\arguments{

  \item{firm.returns}{a \pkg{zoo} time series object (univariate or
  otherwise) with firm returns.}

  \item{market.returns}{a \pkg{zoo} time series of market index returns.}

  \item{residuals}{a \sQuote{logical} indicating whether to return
    residuals or \sQuote{lm} object.}
}

\value{Residual returns unexplained by market index returns.}

\author{Chirag Anand, Vikram Bahure}

\examples{ 
data("StockPriceReturns")
data("OtherReturns")

mm.result <- marketModel(firm.returns = StockPriceReturns,
                         market.returns = OtherReturns$NiftyIndex,
                         residuals = TRUE)

comparison <- merge(MarketModel = mm.result$Infosys,
                    Infosys = StockPriceReturns$Infosys,
                    NiftyIndex = OtherReturns$NiftyIndex,
                    all = FALSE)
plot(comparison)
}

\keyword{marketModel}
