\name{makeX}
\alias{makeX}

\title{Prepare regressors for computation of Augmented Market
  Models}

\description{\sQuote{makeX} is used to prepare regressors for
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
    \pkg{zoo}. In market models, this is normally the returns of a
    stock market index.}

  \item{others}{a \pkg{zoo} matrix of other regressors for the multiple
  regression model.}

  \item{switch.to.innov}{a \sQuote{logical} vector with an element for
    each column in \dQuote{others} specifying whether to switch the
    column from raw values to auto-regressive residuals. Default is \sQuote{TRUE} for
    all the columns. See \sQuote{Details}.}

  \item{market.returns.purge}{a \sQuote{logical} indicating
  whether to purge the effects of \dQuote{others} from
  \dQuote{market.returns}. See \sQuote{Details}.}

 \item{nlags}{ a integer specifying the number of lags required when
   \sQuote{market.returns.purge} is \sQuote{TRUE}.}

 \item{dates}{ a \sQuote{Date} vector, default set to \sQuote{NULL},
 specifying breaks. See \sQuote{Details}.} 

 \item{verbose}{ a \sQuote{logical} value, with the default being
   \sQuote{FALSE}, specifying whether detailed output is required.} 

}

\details{
  This function prepares the regressors of interest for the purpose of
  running aumgmented market models.

  The \sQuote{logical} vector \sQuote{switch.to.innov} is applicable
  only to the regressors in \sQuote{others} matrix and not to the time
  series vector in \sQuote{market.returns}. 
  
  When \sQuote{market.returns.purge} is \sQuote{TRUE}, residuals from a
  regression of \sQuote{market.returns} on \sQuote{others} is
  obtained. If \sQuote{dates} are provided, this regression is done
  within sub-periods as identified by the \sQuote{dates} vector. When
  \sQuote{dates} is set to default value of \sQuote{NULL}, the start and
  end points of \sQuote{market.returns} is taken as the period for
  estimation. 
}

\section{Warning}{The input data should not contain \sQuote{NA}s, as is
  required by the \dQuote{lm} function to estimate a linear regression. Please use
  \sQuote{na.omit} before feeding data into this function.}
  

\value{a time-series object of regressors is returned.}

\author{Ajay Shah, Chirag Anand, Vikram Bahure, Vimal Balasubramaniam}

\examples{
data("OtherReturns")
market.returns <- OtherReturns$NiftyIndex
currency.returns <- OtherReturns$INRUSD

X <- makeX(market.returns,
           others = currency.returns,
           switch.to.innov = FALSE,
           market.returns.purge = FALSE,
           verbose = FALSE)
head(na.omit(X))
}
\keyword{makeX}
