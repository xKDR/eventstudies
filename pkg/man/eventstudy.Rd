\name{eventstudy}
\alias{eventstudy}

\title{Event study analysis}

\description{Function to compute event study inference using different inference strategies for various market models.}

\usage{
eventstudy(firm.returns = NULL,
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
    data on which event study is to be performed.
  }

  \item{eventList}{
    \sQuote{data.frame} of two columns containing event
    dates. \dQuote{outcome.unit} consists of column names of the event
    stock, and \dQuote{event.when} is the respective event date.
  }

  \item{width}{                        % FIXME: needs better description
    \sQuote{integer} of length 1, it studies the performance of observations before and
    after the event
  }

  \item{type}{
    a scalar of type \sQuote{character}, specifying the market
    model adjustment. Currently supported options are:
    \dQuote{marketResidual}, \dQuote{excessReturn}, \dQuote{AMM} and
    \dQuote{None}. Defaults to \dQuote{marketResidual}. See \sQuote{Details}.
  }

  \item{to.remap}{       
    \sQuote{logical}, indicating if remap of the event frame should be
    done.
  }

  \item{remap}{                         % FIXME: list all the possible
                                        % options properly. Needs better
                                        % explanation of the purpose of
                                        % the argument
    \sQuote{character}, this argument is used when \dQuote{to.remap} is
    \sQuote{TRUE} to estimate cumulative sum (cumsum), cumulative
    product (cumprod) or reindex the event frame.
  }

  \item{is.levels}{
    \sQuote{logical}, indicating the format of the data. Set it to
    \sQuote{FALSE} if the data is in returns format else \sQuote{TRUE}.
  }

  \item{inference}{
    \sQuote{logical}, specifying whether to compute confidence interval
    for the estimator.
  }

  \item{inference.strategy}{      % FIXME: list all the possible options
    a \sQuote{character} scalar specifying the inference strategy to
    compute confidence intervals. Is used only if \dQuote{inference} is
    \sQuote{TRUE}. Defaults to \dQuote{bootstrap}.
  }

  \item{...}{
    further arguments to pass to the model.
  }
}

\details{
  When using \dQuote{AMM} adjustement, the default output is
  \dQuote{residual}.
}

\value{ 
  A list with class attribute \sQuote{\code{es}} holding the
  following elements:\cr

  \item{eventstudy.output}{Output provides mean/median estimate with confidence interval}
  \item{outcomes}{A vector has outcome of each event date. It provides 4 outcomes, 'success', 'wrongspan', 'wdatamissing' and 'unitmissing'. 'success' shows the successful use of event date, 'wdatamissing' appears when width data is missing around the event, if event date falls outside the range of physical date then it returns 'wrongspan' and 'unitmissing' when the unit (firm name) is missing in the event list.}
  \item{inference}{Provides information about which inference strategy is utilised to estimate confidence intervals}
  \item{width}{Window width for event study output }
  \item{remap}{It shows which remapping technique used, for eg. 'cumsum' for cumulative sum, 'cumprod' for cumulative product, 'reindex' for reindexing the series and 'none' for no transformation.}
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
es <- eventstudy(firm.returns = StockPriceReturns, eventList = SplitDates, width = 10,type = "None", to.remap = TRUE, remap = "cumsum",inference = TRUE, inference.strategy = "bootstrap")

}

\keyword{eventstudy}
