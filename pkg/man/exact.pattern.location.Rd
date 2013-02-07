\name{exact.pattern.location}
\alias{exact.pattern.location}

\title{
 Gets the location of the pattern
}

\description{
 The function gives the exact location of the exact pattern in the series.
 }

\usage{
exact.pattern.location(us,pt,pt.len)
}

\arguments{
  \item{us}{It is the series in which the location of the pattern is to
    be found}
  \item{pt}{It is the pattern which is to be searched in the series for
    eg. 1}
  \item{pt.len}{It is the length of the pattern which is to be searched
  for}
}

\value{
Output is the row number of the start of the pattern
}


\examples{
# Loading data
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
# Marking left tail and right tail
res <- gen.data(d=input,probvalue=5,value="nonreturns")
# Getting summarised run length
output <- exact.pattern.location(res$left.tail, pt=1, pt.len=2)
}