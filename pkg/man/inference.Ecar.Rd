\name{inference.Ecar}
\alias{inference.Ecar}

\title{
Bootstrap inference for the event study.
}

\description{
This function does bootstrap inference for the entire Ecar, i.e. main graph of the event study.
 }

\usage{
inference.Ecar(z.e,to.plot=FALSE)
}

\arguments{
  \item{z.e}{z.e is the first component of the list returned by the function phys2eventtime.}
  \item{to.plot}{This argument will generate an eventstudy plot of the inference estimated. If to.plot is equal to TRUE then function would generate the plot else it would not. }
}

\value{
A data frame with 3 columns,the lower CI,the mean and the upper CI which are the result of bootstrap inference.
}

\seealso{
phys2eventtime
}

\examples{
data(eventDays)
data(eventstudyData)
eventtime <- phys2eventtime(z=eventstudyData, events=eventDays,width=5)
inference.Ecar(z.e=eventtime, to.plot=FALSE)
}