\name{remap.cumprod}
\alias{remap.cumprod}

\title{Returns geometric returns.}

\description{This function returns the geometric returns for a given zoo object.}

\usage{remap.cumprod(z, is.pc = TRUE, is.returns = TRUE, base = 100)}

\arguments{
  \item{z}{z is the zoo object returned from phys2eventtime.}

  \item{is.pc}{Logical flag if input is a percentage}

  \item{is.returns}{Logical flag if input is returns.}

  \item{base}{The base for the cumulative product.}
}

\value{The frame returned has each column replaced by base * the cumulative product of the column.}

\seealso{
eventDays, eventstudyData, phys2eventtime
}

\examples{
data(eventDays)
data(eventstudyData)
es.results <- phys2eventtime(z=eventstudyData, events=eventDays,width=5)
es.w <- window(es.results$z.e, start=-5, end=+5)
eventtime <- remap.cumprod(es.w, is.pc=FALSE, is.returns=TRUE, base=0)
}

