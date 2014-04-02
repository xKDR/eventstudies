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
    \sQuote{character}, conversion function for remapping event-window
    returns. Used when \dQuote{to.remap} is \sQuote{TRUE}.
  }

  \item{is.levels}{
    \sQuote{logical}, indicating the format of the data. Set it to
    \sQuote{FALSE} if the data is in returns format, else \sQuote{TRUE}.
  }
  %% FIXME: do you compute confidence interval or use it for computing?
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
    \item{\dQuote{marketResidual}: uses \code{\link{marketResidual}}
      function to extract market returns from firms return using a
      linear model.}

    \item{\dQuote{excessReturn}: uses \code{\link{excessReturn}} to
      subtract market return from firm return.}

    \item{\dQuote{lmAMM}: uses \code{\link{lmAMM}} to perform
      Augmented Market Model estimation.}

    \item{\dQuote{None}: don't use any model.}
  }
  Arguments to a model type can be sent inside \sQuote{...}. See
  \sQuote{Model arguments} section for details on accepted fields.

  \dQuote{remap} can take three values:
  \itemize{
    \item \dQuote{cumsum}: cumulative sum, uses \code{\link{remap.cumsum}}. [Default]
    \item \dQuote{cumprod}: cumulative product, buy-hold-abnormal-return (BHAR),
      uses \code{\link{remap.cumprod}}.
    \item \dQuote{reindex}: re-indexes the event window by using
      \code{\link{remap.event.reindex}}.
  }

  For computing confidence intervals, the function can either use
  bootstrap or Wilcoxon signed-rank test. See
  \code{\link{inference.bootstrap}} and \code{\link{inference.wilcox}}
  for more details.

  \sQuote{...} is directly supplied to the model mentioned in the
  \dQuote{type} argument. See section on \sQuote{Model arguments} for
  more details.
}

\section{\bold{Model arguments}}{
  Each model can take extra arguments (supplied as \sQuote{...}) apart
  from mandatory ones for finer control over the analysis. Check the
  respective function documentation for definitions. The arguments are
  listed here:

  \itemize{
    \item \dQuote{marketResidual}: \cr
    - market.returns
    \item \dQuote{excessReturn}: \cr
    - market.returns
    \item \dQuote{lmAMM}: \cr
    - market.returns \cr
    - others \cr
    - switch.to.innov \cr
    - market.returns.purge \cr
    - nlags \cr
    - dates \cr
    - verbose \cr
  }
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
print(es)
plot(es)
}

\keyword{eventstudy}
