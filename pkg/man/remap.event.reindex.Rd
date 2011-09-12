\name{remap.event.reindex}
\alias{remap.event.reindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
This function sets the value at event date to be 100 and the other values relative to this value.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
%%     ~~Describe \code{z} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a zoo object which has been rescaled.



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
phys2eventtime
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
