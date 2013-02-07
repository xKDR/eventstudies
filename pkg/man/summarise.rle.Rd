\name{summarise.rle}
\alias{summarise.rle}

\title{
 Estimating the runlength.
}

\description{
 Summarising runlength in a proper format.
 }

\usage{
summarise.rle(oneseries)
}

\arguments{
  \item{oneseries}{It is the series for which run length is to be summarised}
}

\value{
Output is the min. max and mean values of the run length
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
# Marking left tail and right tail
res <- gen.data(d=input,probvalue=5,value="nonreturns")
# Getting summarised run length
output <- summarise.rle(res$left.tail)
}