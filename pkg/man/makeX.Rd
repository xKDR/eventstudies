\name{makeX}
\alias{makeX}

\title{A function to prepare explanatory variables for computation of
  Augmented Market Models used in lmAMM function.}

\description{This function creates a matrix of explanatory variables in
  the form specified by the user for further estimation of Augmented
  market models.}

\usage{
makeX(market.returns,
      others,
      switch.to.innov = rep(TRUE, NCOL(others)),
      market.returns.purge = TRUE,
      nlags = 5,
      dates = NULL,
      verbose = FALSE)
}

\arguments{
  %% FIXME: our example contains univariate time-series object. Can it
  %% accept multiple columns as input?
  \item{market.returns}{a \pkg{zoo} timeseries object. This is generally
    fixed to the stock market index. The first column vector of this
    matrix contains this variable.  }

  %% FIXME: same as market.returns: can it contain more than one variable?
  \item{others}{A zoo matrix with other regressors of interest in the
    AMM. This could be currency, bond returns, foreign flows, or any
    other variable.}

  \item{switch.to.innov}{a \sQuote{logical} vector with an element for
    each column in \dQuote{others}. Whether to switch the column from
    raw values to AR residuals.}

  %% FIXME: Second part of the sentence (i.e.) is somewhat unclear.
  \item{market.returns.purge}{Whether to purge the effects
    \dQuote{others} from \dQuote{market.returns} (i.e., is it replaced by
    residuals of a model explaining market.returns using all these
   'others').}

 \item{nlags}{The number of lag terms present in this model explaining
   market.returns using all these \dQuote{others}.}

 \item{dates}{Specified break dates as \sQuote{Date} object (either from
   structural breaks in exchange rate regimes) so that all these steps
   are constructed within each sub-period divided by the dates.}

 \item{verbose}{Whether detailed output is required. Default is
   \sQuote{FALSE}}.
}

%% FIXME: Provide a reason why there shouldn't be NAs.
\section{Warning}{The input data should not contain \sQuote{NA}s. Please
  use \sQuote{na.omit} before feeding data into this function.}

\value{This function generates a user specified matrix of explanatory
  variables that will be further used in running Augmented market
  models.}


\author{Ajay Shah, Vimal Balasubramaniam}

\examples{
data("lmAMMData")

market.returns <- lmAMMData$index.nifty
currency.returns <- lmAMMData$currency.inrusd

X <- makeX(market.returns,
           others = currency.returns,
           switch.to.innov = FALSE,
           market.returns.purge = FALSE,
           verbose = FALSE)
}

\keyword{makeX}
