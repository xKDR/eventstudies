\name{phys2eventtime}
\alias{phys2eventtime}

\title{
Converts physical dates to event dates.
}

\description{
Given a zoo time-series vector and an event date, it converts the physical date to the event date. The event date becomes zero and all other dates shift accordingly.
}

\usage{
phys2eventtime(z, events, width = 10)
}

\arguments{
  \item{z}{z is a zoo object containing input data. E.g. this could be all the prices of a bunch of stocks. The column name is the unit name.
events is a data.frame containing 2 columns. The first column ("unit") is the name of the unit. The second column is the date/time ("when") when the event happened.}

  \item{events}{A data.frame containing unit name and corresponding event dates.}

  \item{width}{Width corresponds to the number of days on each side of the event date.}

}

\value{Returns a list containing 1. A zoo object indexed with event time, and having "enough data points" and 2. a vector which describes the status of each unit in the original data ( this maybe more than the no of units in 1 ).}

\author{Ajay Shah, Vimal Balasubramaniam}

\examples{
data(eventDays)
data(eventstudyData)
phys2eventtime(z=eventstudyData, events=eventDays,width=5)	
}

\keyword{ phys2eventime }

