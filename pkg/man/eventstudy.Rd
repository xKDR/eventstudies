\name{eventstudy}
\alias{eventstudy}

\title{Function for performing event study analysis}

\description{
  %% FIXME
  \sQuote{eventstudy} performs event study analysis. It calculates
  returns using a specified model, converts physical dates to event
  frame, and conducts inference on the unit of interest.
}

\usage{
eventstudy(firm.returns,
           eventList,
           width = 10,
           is.levels =  FALSE,
           type = "marketResidual",
           to.remap = TRUE,
           remap = "cumsum",
           inference = TRUE,
           inference.strategy = "bootstrap",
           ...)
}

\arguments{
  \item{firm.returns}{
   a \pkg{zoo} timeseries on which event study is to be performed.
  }

  \item{eventList}{
    \sQuote{data.frame} of two columns containing event dates to pass to
    \code{\link{phys2eventtime}} function. \dQuote{outcome.unit}
    consists of column names of series inside \dQuote{firm.returns}, and
    \dQuote{event.when} is the respective event date.
  }

  \item{width}{integer of length 1, creates an event window to
    study the impact (pre and post event) on the variable.}

  \item{type}{
    a scalar of type \sQuote{character} specifying the market
    model adjustment. See \sQuote{Details}.
  }

  \item{to.remap}{\sQuote{logical}, indicating whether to convert
    event-window returns into cumulative sum, cumulative product, or
    re-index.}

  \item{remap}{
    \sQuote{character}, this argument is used when \dQuote{to.remap} is
    \sQuote{TRUE}. Takes three values (defaults to \dQuote{cumsum}):
    \itemize{
      \item{cumsum: cumulative sum.}
      \item{cumprod: cumulative product, buy-hold-abnormal-return (BHAR).}
      \item{reindex: re-indexing the event window.}
    }
  }

  \item{is.levels}{
    \sQuote{logical}, indicating the format of the data. Set it to
    \sQuote{FALSE} if the data is in returns format, else \sQuote{TRUE}.
  }

  \item{inference}{
    \sQuote{logical}, specifying whether to compute confidence interval
    for the estimator.
  }

  \item{inference.strategy}{a \sQuote{character} scalar specifying the
    inference strategy to compute confidence intervals. Is used only if
    \dQuote{inference} is \sQuote{TRUE}. Currently supported options are
    \dQuote{bootstrap} and \dQuote{wilcox}. Defaults to \dQuote{bootstrap}.}

  \item{...}{
    further arguments to pass to the model.
  }
}

\details{

  \dQuote{firm.returns} may contain only one series. For performing
  analysis for a single series, use \sQuote{[} with \code{drop =
  FALSE} for subsetting the data set. See \code{\link{phys2eventtime}}
  for more details.

  \dQuote{type} currently supports:
  \itemize{
    \item{\dQuote{marketResidual}: uses \code{\link{marketResidual}()} to
      extract market returns from firms return using a linear model.}

    \item{\dQuote{excessReturn}: uses \code{\link{excessReturn}()} to
      subtract market return from firm return.}

    \item{\dQuote{lmAMM}: uses \code{\link{lmAMM}()} to perform
      Augmented Market Model estimation.}

    \item{\dQuote{None}: don't use any model.}
  }

    
  The function needs firm returns as input and provides
  argument \sQuote{type} to generate abnormal returns, if required. If
  \sQuote{type} is \sQuote{market.residuals} then it will generate
  abnormal returns using a market model. It also provides option to
  estimate returns using augmented market model (AMM) which purges out
  market returns and \sQuote{others} i.e. currency in our case
  (\sQuote{type} as \sQuote{lmAMM}). The function computes confidence
  interval using either bootstrap or wilcoxon technique. The output of
  this function has class \sQuote{es}.


}

\value{
  A list with class attribute \dQuote{es} holding the
  following elements:

  \item{eventstudy.output}{
    a \sQuote{matrix} containing mean/median estimate with
    confidence interval; \sQuote{NULL} if there are no \dQuote{success}
    \dQuote{outcomes}.
  }

  \item{outcomes}{a character vector having outcome of each event date:}

  \itemize{
    \item{success: shows the successful use of event date.}
    \item{wdatamissing: appears when width data is missing around the event.}
    \item{wrongspan: if event date falls outside the range of physical date.}
    \item{unitmissing: when the unit (firm name) is missing in the event list.}
  }

  \item{inference}{
    a \sQuote{character} providing information about which inference
    strategy was utilised to estimate the confidence intervals.
  }

  \item{width}{
    a \sQuote{numeric} specifying the window width for event study output.
  }

  \item{remap}{
    a \sQuote{character} specifying the remapping technique
    used. Options are mentioned in \dQuote{remap} argument description.
  }

}

\author{Vikram Bahure}

\seealso{ \code{\link{lmAMM}},
  \code{\link{marketResidual}},	
  \code{\link{excessReturn}},
  \code{\link{phys2eventtime}},
  \code{\link{inference.bootstrap}},
  \code{\link{inference.wilcox}},
  \code{\link{remap.cumsum}},
  \code{\link{remap.cumprod}},
  \code{\link{remap.event.reindex}},
}

\examples{ 
## Performing event study
library(eventstudies)
data("StockPriceReturns")
data("SplitDates")

## Event study without adjustment
es <- eventstudy(firm.returns = StockPriceReturns,
                 eventList = SplitDates,
                 width = 10,
                 type = "None",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap")
}

\keyword{eventstudy}
