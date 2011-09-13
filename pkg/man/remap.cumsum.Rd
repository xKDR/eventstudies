\name{remap.cumsum}
\alias{remap.cumsum}
\title{
This function gives the cumulative total of the values ( in level or percentage form ).
}
\description{
This function gives the cumulative total of the values. 



}
\usage{
remap.cumsum(z, is.pc = TRUE, base = 0)
}
\arguments{


  \item{z}{
z is the zoo object returned from phys2eventtime.
}
  \item{is.pc}{
is.pc is a logical flag to indicate that the values are in percentage form.
}
  \item{base}{
base (default 0) for the cumulative sum.

}
}
\value{
The frame returned has each column replaced by the cumulative sum of the column.

}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (z, is.pc = TRUE, base = 0) 
{
    for (i in 1:ncol(z)) {
        tmp <- z[, i]
        if (is.pc) {
            tmp <- tmp/100
        }
        z[, i] <- base + cumsum(tmp)
    }
    z
  }
}

