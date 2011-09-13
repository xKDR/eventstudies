\name{remap.event.reindex}
\alias{remap.event.reindex}
\title{
This function sets the value at event date to be 100 and the other values relative to this value.
}
\description{

A function which consumes a zoo object where there are lots of events
(as columns)
The contents are all levels
For each column, the event date value is set to 100 and all other
values are scaled accordingly.

It's input is a zoo object which is the first component of the list returned by phys2eventtime.

}
\usage{
remap.event.reindex(z)
}
\arguments{
  \item{z}{
}
}
\value{
The function returns a zoo object which has been rescaled.
}

\seealso{
phys2eventtime
}
\examples{

## The function is currently defined as
function (z) 
{
    eventvals <- as.numeric(window(z, start = 0, end = 0))
    for (i in 1:ncol(z)) {
        z[, i] <- 100 * z[, i]/eventvals[i]
    }
    z
  }
}

