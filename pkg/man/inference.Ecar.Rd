\name{inference.Ecar}
\alias{inference.Ecar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function does bootstrap inference for the entire Ecar, i.e. main graph of the event study.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
z.e is a zoo object with certain rows (e.g. from -10 to 10)
that define the event window, and columns with data for units.
 }
\usage{
inference.Ecar(z.e)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z.e}{
z.e is the first component of the list returned by the function phys2eventtime.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
