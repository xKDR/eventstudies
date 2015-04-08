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
           event.list,
           event.window = 10,
           is.levels =  FALSE,
           type = "marketModel",
           to.remap = TRUE,
           remap = "cumsum",
           inference = TRUE,
           inference.strategy = "bootstrap",
           model.args = NULL)
}

\arguments{
  \item{firm.returns}{
    a \pkg{zoo} matrix of \sQuote{outcome} or \sQuote{response} series. 
  }

  \item{event.list}{
    a \code{data.frame} of two columns with event dates (colname:
    \dQuote{when}) and column names of the \sQuote{response}
    series from \sQuote{firm.returns} (colname \dQuote{name}).
  }

  \item{event.window}{an \sQuote{integer} of length 1 that specifies a
    symmetric event window around the event time as specified in the
    \code{index} of \dQuote{firm.returns}.
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
  
  \item{model.args}{
    All other arguments to be passed depends on whether \sQuote{type}
    is \dQuote{marketModel}, \dQuote{excessReturn}, or
    \dQuote{lmAMM}. When \dQuote{None}, no additional arguments will be
    needed. See \sQuote{Details}. 
  }
}

\details{ This function is used to conduct event study analysis acting
  as a wrapper over the functionality provided in the \pkg{eventstudies}
  package. It provides an interface to select and control the process of
  event study analysis. It includes choice of the statistical model for
  doing in-sample estimation and computing coefficients, choice of
  cumulative returns, and selection of inference procedure. Process used
  to conduct a study is detailed below:

  \enumerate{
    \item{event.period: is defined as (-event.window, event.window].}

    \item{estimation.period: If \dQuote{type} is specified, then
      estimation.period is calculated for each firm-event in
      \dQuote{event.list}, starting from the start of the data span till
      the start of event period (inclusive).}

    \item{For each firm-event, firm returns and other returns (as
      applicable) are converted to event time using
      \sQuote{phys2eventtime}. Data is merged using \sQuote{merge.zoo}
      to make sure the indexes are consistent before conversion to event
      time.}

    \item{The selected model \dQuote{type} is run on the series indexed
      by event time and abnormal returns are computed.}

    \item{NULL values because of estimation data missing are removed
      from the output and \dQuote{outcomes} object is updated with
      \dQuote{edatamissing}.}

    \item{Remapping is done if \dQuote{to.remap} is \sQuote{TRUE} using
      the function specified in \dQuote{remap} argument.}

    \item{Means of returns are computed across various events.}

    \item{Inference is done if \dQuote{inference} is \sQuote{TRUE} using the
      technique specified in \dQuote{inference.strategy}.}
  }

  \dQuote{firm.returns} can contain a single series also. To study a single
  series, use \sQuote{[} with \code{drop = FALSE} to subset the
  data set. See \code{\link{phys2eventtime}} for more details.

  \sQuote{NA} values in the returns data are converted to \code{0}.

  \dQuote{type} currently supports:
  \itemize{
    \item{\dQuote{marketModel}: uses \code{\link{marketModel}}
      function to regress market returns on firms return using a
      linear model.}

    \item{\dQuote{excessReturn}: uses \code{\link{excessReturn}} to
      subtract market return from firm return.}

    \item{\dQuote{lmAMM}: uses \code{\link{lmAMM}} to perform
      Augmented Market Model estimation.}

    \item{\dQuote{None}: does not use any model.}
  }
  
  Arguments to a model type can be sent inside \sQuote{model.args}. See
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

  \sQuote{model.args} is directly supplied to the model mentioned in the
  \dQuote{type} argument. See section on \sQuote{Model arguments} for
  more details.

  Note: \code{\link{phys2eventtime}} is called with \sQuote{width} set
  to 0 when called from this function.
}

\section{\bold{Model arguments}}{
  Each model can take extra arguments (supplied as \sQuote{model.args}) apart
  from mandatory ones for finer control over the analysis. Check the
  respective function documentation for definitions. The arguments from
  the relevant functions are listed here:

  \itemize{
    \item \dQuote{marketModel}: \cr
    - market.returns
    \item \dQuote{excessReturn}: \cr
    - market.returns
    \item \dQuote{lmAMM}: \cr
    - market.returns \cr
    - others \cr
    - switch.to.innov \cr
    - market.returns.purge \cr
    - nlag.makeX \cr
    - nlag.lmAMM \cr
    - dates \cr
    - verbose \cr

    Note: arguments (except nlag.lmAMM) are directly passed to \sQuote{makeX}, see
    \code{\link{lmAMM}} for more details. \cr
  }
}

\value{
  A list with class attribute \dQuote{es} holding the following
  elements, or \sQuote{NULL} if output from a model function is
  \sQuote{NULL}:

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
          argument \sQuote{width} in \code{\link{phys2eventtime}} is set to 0.}
        \item{wrongspan: if event date falls outside the range of physical date.}
        \item{unitmissing: when the unit (firm name) is missing in the
          event list.}
        \item{edatamissing: when there is insufficient data to do model
        estimation.}
      }
    }
  }

  The returned object contains input information in other attributes:
  \itemize{
    \item{\dQuote{model.residuals}:}{
      a \sQuote{list} of residual series as returned by the selected
      model. For models which do not compute residuals, this attribute is
      not returned.}

    \item{\dQuote{CAR}:}{
      a \sQuote{zoo} object contaning Cumulative Abnormal Returns as
      returned by the function specified by \dQuote{remap} argument.
    }

    \item{\dQuote{inference}:}{
      a \sQuote{character} providing information about which inference
      strategy was utilised to estimate the confidence intervals.
    }

    \item{\dQuote{inference.strategy}:}{
      a \sQuote{character} providing the name of the selected model.
    }

    \item{\dQuote{event.window}:}{
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
  \code{\link{marketModel}},	
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
                 event.list = SplitDates,
                 event.window = 7,
                 type = "None",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap")
str(es)
plot(es)

                # Event study using Market Model
es <- eventstudy(firm.returns = StockPriceReturns,
                 event.list = SplitDates,
                 event.window = 7,
                 type = "marketModel",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                 model.args = list(
                        market.returns = OtherReturns[, "NiftyIndex"]
                        )
                 )
str(es)
plot(es)

                # Event study using Augmented Market Model
es <- eventstudy(firm.returns = StockPriceReturns,
                 event.list = SplitDates,
                 event.window = 7,
                 type = "lmAMM",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap",
                                                 # model arguments
                 model.args = list(
                        market.returns = OtherReturns[, "NiftyIndex"],
                        others = OtherReturns[, "USDINR"],
                        market.returns.purge = TRUE,
                        nlag.makeX = 5,
                        nlag.lmAMM = 5
                        )
                 )
str(es)
plot(es)
}

\keyword{eventstudy}
