\name{remap.cumprod}
\alias{remap.cumprod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns geometric returns.
}
\description{
This function returns the geometric returns for a given zoo object.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
remap.cumprod(z, is.pc = TRUE, is.returns = TRUE, base = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
z is the zoo object returned from phys2eventtime.
%%     ~~Describe \code{z} here~~
}
  \item{is.pc}{
Logical flag if input is a percentage
%%     ~~Describe \code{is.pc} here~~
}
  \item{is.returns}{
Logical flag if input is returns.
%%     ~~Describe \code{is.returns} here~~
}
  \item{base}{
The base for the cumulative product.
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
