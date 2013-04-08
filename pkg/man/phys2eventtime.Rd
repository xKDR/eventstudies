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
  \item{z}{Time series data for which event frame is to be generated.}

  \item{events}{It is a data frame with two columns: unit and when. unit has column name of which response is to measured on the event date, while when has the event date.}

  \item{width}{Width corresponds to the number of days on each side of the event date.For a given width, if there is any NA in the event window then the last observation is carried forward.}

}

\value{Output is in a list format. A zoo object indexed with event time and a vector which describes the status of each unit in the original data.
}


\examples{
data(eventDays)
data(eventstudyData)
phys2eventtime(z=eventstudyData, events=eventDays,width=5)	
}

\keyword{ phys2eventime }

