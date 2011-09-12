\name{rawres}
\alias{rawres}
\docType{data}
\title{
Sample result for testing the package
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(rawres)}
\format{
  The format is:
List of 2
 $ z.e     :'zoo' series from -9 to 9
  Data: num [1:19, 1:3] NA NA NA NA NA ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : chr [1:3] "1" "2" "3"
  Index:  int [1:19] -9 -8 -7 -6 -5 -4 -3 -2 -1 0 ...
 $ outcomes: Factor w/ 3 levels "success","unitmissing",..: 1 1 1 3 3 2
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(rawres)
## maybe str(rawres) ; plot(rawres) ...
}
\keyword{datasets}
