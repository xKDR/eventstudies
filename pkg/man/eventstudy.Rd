\name{eventstudy}
\alias{eventstudy}

\title{Perform event study analysis}

\description{
  \sQuote{eventstudy} provides an easy interface that integrates all
  functionalities of package \pkg{eventstudies} to undertake event study
  analysis. It allows the user to specify the type of data adjustment
  to be done (using market model functionalities of the
  package) and then an inference strategy of choice.
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
    a \pkg{zoo} matrix of \sQuote{outcome} or \sQuote{response} series. 
  }

  \item{eventList}{
    a \code{data.frame} of two columns with event dates (colname:
    \dQuote{when}) and column names of the \sQuote{response}
    series from \sQuote{firm.returns} (colname \dQuote{name}).
  }

  \item{width}{an \sQuote{integer} of length 1 that specifies a
    symmetric event window around the event date.
  }

  \item{type}{
    a scalar of type \sQuote{character} specifying the type of data
    adjustment required before conducting an event study analysis. See
    \sQuote{Details}.
  }

  \item{to.remap}{\sQuote{logical}, indicating whether or not to remap
    the data in \sQuote{firm.returns}.
  }

  \item{remap}{
    \sQuote{character}, indicating the type of remap required, 
    \dQuote{cumsum}, \dQuote{cumprod}, or \dQuote{reindex}. Used when
    \sQuote{to.remap} is \sQuote{TRUE}. 
   }

  \item{is.levels}{
    \sQuote{logical}, indicating whether data in \sQuote{firm.returns}
    needs to be converted into percentage returns. If
    \sQuote{TRUE}, \sQuote{firm.returns} will be converted into
    percentage returns. 
  }

  \item{inference}{
    \sQuote{logical}, specifying whether to undertake statistical
    inference and compute confidence intervals. The default setting is
    \sQuote{TRUE}. 
  }

  \item{inference.strategy}{a \sQuote{character} scalar specifying the
    inference strategy to be used for estimating the confidence
    interval. Presently, two methods are available: \dQuote{bootstrap}
    and \dQuote{wilcox}.
  }
  
  \item{...}{
    All other arguments to be passed depends on whether \sQuote{type}
    is \dQuote{marketResidual}, \dQuote{excessReturn}, or
    \dQuote{lmAMM}. When \dQuote{None}, no additional arguments will be
    needed. See \sQuote{Details}. 
  }
}

\details{
  \dQuote{firm.returns} can contain only one series. To study a single
  series, use \sQuote{[} with \code{drop = FALSE} for subsetting the
  data set. See \code{\link{phys2eventtime}} for more details.

  \sQuote{NA} values in the returns data are converted to \code{0}.

  \dQuote{type} currently supports:
  \itemize{
    \item{\dQuote{marketResidual}: uses \code{\link{marketResidual}}
      function to extract market returns from firms return using a
      linear model.}

    \item{\dQuote{excessReturn}: uses \code{\link{excessReturn}} to
      subtract market return from firm return.}

    \item{\dQuote{lmAMM}: uses \code{\link{lmAMM}} to perform
      Augmented Market Model estimation.}

    \item{\dQuote{None}: does not use any model.}
  }
  
  Arguments to a model type can be sent inside \sQuote{...}. See
  \sQuote{Model arguments} section for details on accepted
  fields.

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

  \code{\link{phys2eventtime}} is called with \sQuote{width} set to
  zero when called from this function.
}

\section{\bold{Model arguments}}{
  Each model can take extra arguments (supplied as \sQuote{...}) apart
  from mandatory ones for finer control over the analysis. Check the
  respective function documentation for definitions. The arguments from
  the relevant functions are listed here:

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

    Note: arguments are directly passed to \sQuote{makeX}, see
    \code{\link{lmAMM}} for more details. \cr
  }
}

\value{
  A list with class attribute \dQuote{es} holding the following
  elements:

  \itemize{
    \item{\dQuote{eventstudy.output}:}{
      a \sQuote{matrix} containing mean (bootstrap) or median (with
      wilcoxon) estimate with confidence interval; \sQuote{NULL} if there
      are no \dQuote{success} \dQuote{outcomes}.
    }

    \item{\dQuote{outcomes}:}{
      a character vector that is the output from
      \code{\link{phys2eventtime}} containing details of the successful use
      of an event:
      
      \itemize{
        \item{success: shows the successful use of event date.}
        \item{wdatamissing: appears when width data is missing around the
          event. This will not appear when this function is used since the
          argument \sQuote{width} in \code{\link{phys2eventtime}} is set to zero.}
        \item{wrongspan: if event date falls outside the range of physical date.}
        \item{unitmissing: when the unit (firm name) is missing in the event list.}
      }
    }
  }

  The returned object contains input information in other attributes:
  \itemize{
    \item{\dQuote{inference}:}{
      a \sQuote{character} providing information about which inference
      strategy was utilised to estimate the confidence intervals.
    }

    \item{\dQuote{width}:}{
      a \sQuote{numeric} specifying the window width for event study output.
    }

    \item{\dQuote{remap}:}{
      a \sQuote{character} specifying the remapping technique
      used. Options are mentioned in \dQuote{remap} argument description.
    }
  }

  Function \sQuote{print.es} is provided to print the coefficients and
  exposures of the analysis. \sQuote{plot.es} is used to plot the model
  residuals and firm returns.
}

\author{Ajay Shah, Chirag Anand, Vikram Bahure, Vimal Balasubramaniam}

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
data("StockPriceReturns")
data("SplitDates")
data("OtherReturns")

                # Event study without adjustment
es <- eventstudy(firm.returns = StockPriceReturns,
                 eventList = SplitDates,
                 width = 10,
                 type = "None",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap")
str(es)
plot(es)

                # Event study using Market Model
es <- eventstudy(firm.returns = StockPriceReturns,
                 eventList = SplitDates,
                 width = 10,
                 type = "marketResidual",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                 market.returns = OtherReturns[, "NiftyIndex"],
                 )
str(es)
plot(es)

                # Event study using Augmented Market Model
events <- data.frame(name = c("Infosys", "TCS"),
                     when = c("2012-04-01", "2012-06-01"),
                     stringsAsFactors = FALSE)

es <- eventstudy(firm.returns = StockPriceReturns,
                 eventList = events,
                 width = 10,
                 type = "lmAMM",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                                                 # model arguments
                 market.returns = OtherReturns[, "NiftyIndex"],
                 others = OtherReturns[, c("USDINR", "CallMoneyRate")],
                 market.returns.purge = TRUE, nlags = 1
                 )
str(es)
plot(es)
}

\keyword{eventstudy}
