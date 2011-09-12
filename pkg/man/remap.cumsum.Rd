\name{remap.cumsum}
\alias{remap.cumsum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function gives the cumulative total of the values ( in level or percentage form ).
%%  ~~function to do ... ~~
}
\description{
This function gives the cumulative total of the values. 


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
remap.cumsum(z, is.pc = TRUE, base = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{


  \item{z}{
z is the zoo object returned from phys2eventtime.
%%     ~~Describe \code{z} here~~
}
  \item{is.pc}{

is.pc is a logical flag to indicate that the values are in percentage form.
%%     ~~Describe \code{is.pc} here~~

}
  \item{base}{
base (default 0) for the cumulative sum.

%%     ~~Describe \code{base} here~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
