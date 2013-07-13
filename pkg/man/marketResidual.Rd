\name{marketResidual}
\alias{marketResidual}

\title{This function extracts market return from the firm return }

\description{ This function extracts market return using regression from the firm return to get the residual return
}

\usage{marketResidual(mm.formula=NULL, data.object)
}

\arguments{
  \item{data.object}{Single time series object with all the variables}
  \item{mm.formula}{Here the input is the linear model (lm) formula for eg: a ~ b + c
                    If formula is not given then first column will be treated as 
                    dependent and rest will be independent}

}
\value{ Market residual after extracting market returns from the firm return
}

\author{Vikram Bahure}

\examples{ 
data(marketmodelData)
# Forumla for market model
mm.formula <- paste("ranbaxyacp","~","nifty","+","drug",sep="")
# Extracting market residual
mm.result <- marketResidual(mm.formula=mm.formula,data.object=mmData)

}

\keyword{marketResidual}