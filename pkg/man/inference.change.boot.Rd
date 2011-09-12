\name{inference.change.boot}
\alias{inference.change.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 This does bootstrap inference for the difference in the
 average "car" between t1 and t2 (both in event time).
%%  ~~function to do ... ~~
}
\description{

 z.e is a zoo object, where rows are in event time
 and columns are units of observation.
 Sampling with replacement is done within the units of
 observation. Each time, the Ecar(t1) and Ecar(t2) is
 computed.
 By default, the statistic of interest is the ratio
 Ecar(t2)/Ecar(t1)
 But if operator="difference" is sent in, then the
 statistic of interest shifts to Ecar(t2)-Ecar(t1).

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inference.change.boot(z.e, t1, t2, operator = "ratio", conf = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z.e}{
z is the zoo object returned from phys2eventtime.
%%     ~~Describe \code{z.e} here~~
}
  \item{t1}{
Start of the event period.
%%     ~~Describe \code{t1} here~~
}
  \item{t2}{
End of the event period.
%%     ~~Describe \code{t2} here~~
}
  \item{operator}{
%%     ~~Describe \code{operator} here~~
}
  \item{conf}{
%%     ~~Describe \code{conf} here~~
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
function (z.e, t1, t2, operator = "ratio", conf = 0.95) 
{
    stopifnot(operator \%in\% c("ratio", "difference"))
    tmp <- t(as.matrix(z.e[, c(t1, t2)]))
    if (operator == "ratio") {
        change <- tmp[, 2]/tmp[, 1]
    }
    if (operator == "difference") {
        change <- tmp[, 2] - tmp[, 1]
    }
    mymean <- function(x, d) {
        mean(x[d], na.rm = TRUE)
    }
    b <- boot(change, mymean, R = 1000)
    ci <- boot.ci(b, type = "bca", conf = conf)
    list(est = b$t0, lo = ci$bca[1, 4], hi = ci$bca[1, 5])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
