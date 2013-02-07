\name{gen.data}
\alias{gen.data}

\title{
Marking upper and lower tail events for extreme event analysis.
}

\description{
This function generates a column as left.tail and right.tail which has
binary numbers. If the observation belongs to left tail then the
left.tail will be 1 else 0. 
 }

\usage{
gen.data(d,probvalue,value)
}

\arguments{
  \item{d}{'d' is the time-series on which extreme event analysis is
    done.} 
  \item{probvalue}{It is tail value for which the extreme event is to
    be defined. For eg: prob.value of 5 will consider 5\% tail on both
    sides.}
  \item{value}{If the series 'd' is in returns format then
  value="nonreturns" else value="returns"}
}

\value{
  Output is a data frame with columns as date, value, returns, left.tail
  which is left tail dummy simmilarly for right tail and both tails.

}


\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
# Marking left tail and right tail
res <- gen.data(d=input,probvalue=5,value="nonreturns")
}