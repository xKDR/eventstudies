\name{remap.cumprod}
\alias{remap.cumprod}

\title{
Returns geometric returns.
}
\description{
This function returns the geometric returns for a given zoo object.

}
\usage{
remap.cumprod(z, is.pc = TRUE, is.returns = TRUE, base = 100)
}

\arguments{
  \item{z}{
z is the zoo object returned from phys2eventtime.
}
  \item{is.pc}{
Logical flag if input is a percentage
}
  \item{is.returns}{
Logical flag if input is returns.
}
  \item{base}{
The base for the cumulative product.
}
}

\value{
The frame returned has each column replaced by base * the cumulative product of the column.
}
\examples{

## The function is currently defined as
function (z, is.pc = TRUE, is.returns = TRUE, base = 100) 
{
    for (i in 1:ncol(z)) {
        tmp <- z[, i]
        if (is.returns) {
            if (is.pc) {
                tmp <- tmp/100
            }
            tmp <- 1 + tmp
        }
        tmp[1] <- base
        z[, i] <- cumprod(tmp)
    }
  }
}

