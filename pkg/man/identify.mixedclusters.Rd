\name{identify.mixedclusters}
\alias{identify.mixedclusters}

\title{
 Checks the mixed clusters in the series and generates a dummy for the same
}

\description{
 Checks the mixed clusters in the series and generates a dummy for the same
 }

\usage{
identify.mixedclusters(m,j)
}

\arguments{
  \item{m}{It is the series in which we check for mixed clusters}
  \item{j}{It is the length of cluster we are checking for}
}

\value{
Output adds a column with a dummy for mixed clusters in the series
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
# Marking left tail and right tail
res <- gen.data(d=input,probvalue=5,value="nonreturns")
# exact location of patter
patrn <- exact.pattern.location(res$both.tails, pt=1, pt.len=2)
res$pattern <- FALSE
res$pattern[patrn] <- TRUE
# Getting summarised run length
output <- identify.mixedclusters(m=res,j=2)
}