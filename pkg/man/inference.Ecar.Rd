\name{inference.Ecar}
\alias{inference.Ecar}

\title{
This function does bootstrap inference for the entire Ecar, i.e. main graph of the event study.
}
\description{
 }
\usage{
inference.Ecar(z.e)
where z.e is a zoo object with certain rows (e.g. from -10 to 10)
that define the event window, and columns with data for units.
}
\arguments{
  \item{z.e}{
z.e is the first component of the list returned by the function phys2eventtime.
}
}
\details{
}
\value{
  A data frame with 3 columns,the lower CI,the mean and the upper CI which are the result of bootstrap 
inference.
}


\seealso{
phys2eventtime
}
\examples{

## The function is currently defined as
function (z.e) 
{
    Ecar <- function(transposed, d) {
        colMeans(transposed[d, ], na.rm = TRUE)
    }
    tmp <- t(as.matrix(z.e))
    b <- boot(tmp, Ecar, R = 1000)
    results <- NULL
    for (i in 1:ncol(b$t)) {
        results <- rbind(results, quantile(b$t[, i], prob = c(0.025, 
            0.975)))
    }
    results <- cbind(results[, 1], b$t0, results[, 2])
    rownames(results) <- rownames(z.e)
    colnames(results) <- c("2.5\%", "Mean", "97.5\%")
    results
  }
}

