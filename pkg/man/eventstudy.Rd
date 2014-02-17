\name{eventstudy}
\alias{eventstudy}

\title{Event study analysis}

\description{This function computes event study inference using different inference strategies for various market models}

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
  \item{firm.returns}{Data on which event study is to be performed}
  \item{eventList}{A data frame with event dates. It has two columns 'outcome.unit' and 'event.when'. The first column 'outcome.unit' consists of column names of the event stock and 'event.when' is the respective event date}
  \item{width}{It studies the performance of observations before and after the event}
  \item{type}{This argument gives an option to use different market model adjustment like "marketResidual", "excessReturn", "AMM" and "None"}
  \item{to.remap}{If TRUE then remap the event frame is done}
  \item{remap}{This argument is used when to.remap is TRUE to estimate cumulative sum (cumsum), cumulative product (cumprod) or reindex the event frame}
  \item{is.levels}{If the data is in returns format then is.levels is FALSE else TRUE}
  \item{inference}{This argument is used to compute confidence interval for the estimator}
  \item{inference.strategy}{If inference is TRUE then this argument gives an option to select different inference strategy to compute confidence intervals. Default to bootstrap.}
  \item{...}{Accepts specific arguments for the model.}
}

\details{If type = "AMM", then the default output is "residual".
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
## library(eventstudies)
## data("StockPriceReturns")
## data("SplitDates")

## Event study without adjustment
es <- eventstudy(firm.returns = StockPriceReturns, eventList = SplitDates, width = 10,type = "None", to.remap = TRUE, remap = "cumsum",inference = TRUE, inference.strategy = "bootstrap")

}

\keyword{eventstudy}
