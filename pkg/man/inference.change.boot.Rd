\name{inference.change.boot}
\alias{inference.change.boot}
\title{
This does bootstrap inference for the difference in the
average "car" between t1 and t2 (both in event time).
}

\description{
z.e is a zoo object, where rows are in event time
and columns are units of observation. Sampling with replacement is done within the units of observation. Each time, the Ecar(t1) and Ecar(t2) is computed.

By default, the statistic of interest is the ratio
Ecar(t2)/Ecar(t1)
But if operator="difference" is sent in, then the
statistic of interest shifts to Ecar(t2)-Ecar(t1).
}

\usage{
inference.change.boot(z.e, t1, t2, operator = "ratio", conf = 0.95)
}

\arguments{
  \item{z.e}{z is the zoo object returned from phys2eventtime.}
  \item{t1}{Start of the event period.}
  \item{t2}{End of the event period.}
  \item{operator}{One of ratio or difference to compute the ratio or difference
for the change in Ecar.}
  \item{conf}{The level of confidence for computing the confidence intervals.}
}

\value{It returns a list containing the mean,and the confidence intervals for the change in Ecar.}

\examples{
data(eventDays)
data(eventstudyData)
es.results <- phys2eventtime(z=eventstudyData, events=eventDays,width=5)
es.w <- window(es.results$z.e, start=-5, end=+5)
eventtime <- remap.cumsum(es.w, is.pc=FALSE, base=0)
}
