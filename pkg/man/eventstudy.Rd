\name{eventstudy}
\alias{eventstudy}

\title{Performs event study using different methods and computes confidence intervals using different inference models}

\description{ This function generates event study output and further computes confidence intervals using bootstrap or wilcox method.
}

\usage{
eventstudy(inputData = NULL,
                       eventList,
                       width = 10,
                       levels =  FALSE,
                       type = "marketResidual",
                       to.remap = TRUE,
                       remap = "cumsum",
                       inference = TRUE,
                       inference.strategy = "bootstrap",
                       to.plot = TRUE,
                       xlab = "Event time",
                       ylab = "Cumulative returns of response series",
                       main = "Event study plot", ...)
}

\arguments{
  \item{inputData}{Data on which event study is to be performed}
  \item{eventList}{A data frame with event dates. It has two columns 'unit' and 'when'. The first column 'unit' consists of column names of the event stock and 'when' is the respective event date}
  \item{width}{It studies the performance of observations before and after the event}
  \item{type}{This argument gives an option to use different market model adjustment like "marketResidual", "excessReturn", "AMM" and "None"}
  \item{to.remap}{If TRUE then remap the event frame is done}
  \item{remap}{This argument is used when to.remap is TRUE to estimate cumulative sum (cumsum), cumulative product (cumprod) or reindex the event frame}
  \item{levels}{If the data is in returns format then levels is FALSE else TRUE}
  \item{inference}{This argument is used to compute confidence interval for the estimator}
  \item{inference.strategy}{If inference is TRUE then this argument gives an option to select different inference strategy to compute confidence intervals. Default to bootstrap.}
  \item{to.plot}{This argument will generate an eventstudy plot of the inference estimated. If to.plot is equal to TRUE then function would generate the plot else it would not. }
  \item{xlab}{If to.plot is TRUE then the plot generated will take this X label}
  \item{ylab}{If to.plot is TRUE then the plot generated will take this Y label}	
  \item{main}{If to.plot is TRUE then the plot generated will take this as main title}
  \item{...}{Accepts specific arguments for the model.}
}
\value{ Output is mean estimate  of abnormal returns and confidence interval using particular inference strategy
}

\author{Vikram Bahure}

\seealso{ \code{\link{AMM}},
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
es <- eventstudy(inputData = StockPriceReturns, eventList = SplitDates, width = 10,
                 type = "None", to.remap = TRUE, remap = "cumsum", to.plot = FALSE,
                 inference = TRUE, inference.strategy = "bootstrap")

}

\keyword{eventstudy}