\name{makeX}
\alias{makeX}

\title{Prepare explanatory variables for computation of Augmented Market
  Models}

\description{\sQuote{makeX} is used to prepare explanatory variables for
  computation of Augmented Market Models. It can be used to create a
  matrix of explanatory variables in the form specified by the user for
  estimation of AMM.}

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

  \item{market.returns}{a univariate timeseries object of \sQuote{class}
    \pkg{zoo}. It is generally fixed to the stock market index.}

  \item{others}{a \pkg{zoo} matrix with other regressors of interest in the
    AMM. This could be currency, bond returns, foreign flows, or any
    other variable.}

  \item{switch.to.innov}{a \sQuote{logical} vector with an element for
    each column in \dQuote{others} specifying whether to switch the
    column from raw values to AR residuals. Default is \sQuote{TRUE} for
    all the columns.}

  \item{market.returns.purge}{a \sQuote{logical} indicating
  whether to purge the effects of \dQuote{others} from
  \dQuote{market.returns}.}

 \item{nlags}{The number of lag terms present in this model explaining
   \dQuote{market.returns} using all these \dQuote{others}.}

 \item{dates}{Specified break dates as \sQuote{Date} object.}

 \item{verbose}{Whether detailed output is required. Default is
   \sQuote{FALSE}.}
}

\details{

  %% FIXME
  \dQuote{dates} can be used to specify sub-periods for estimating
  exposure of \dQuote{others}. The variables are constructed for each
  sub-period separately. If no dates are provided, start date and end
  date of the \dQuote{market.returns} series are taken.
}

\section{Warning}{The input data should not contain \sQuote{NA}s, as is
  required by the \dQuote{lm} function to compute linear estimates. Please use
  \sQuote{na.omit} before feeding data into this function.}
  

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
