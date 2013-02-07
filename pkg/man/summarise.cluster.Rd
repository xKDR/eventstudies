\name{summarise.cluster}
\alias{summarise.cluster}

\title{
 Estimating the runlength.
}

\description{
 Summarising runlength in a proper format for left tail, right tail and
 both tails..
 }

\usage{
summarise.cluster(obj)
}

\arguments{
  \item{obj}{It is the series for which run lenght is to summarised. It
  is the output of gen.data function}
}

\value{
Output is the min. max and mean values of the run length
}

\seealso{
  summarise.rle
}

\examples{
data(IdentifyExevent)
# Series input
input <- diff(log(input.data[,"sp500"]))
# Marking left tail and right tail
res <- gen.data(d=input,probvalue=5,value="nonreturns")
# Getting summarised run length
output <- summarise.cluster(res)
}