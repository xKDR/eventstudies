\name{phys2eventtime}
\alias{phys2eventtime}
\title{
Converts physical dates to event dates.
}
\description{
Given a zoo time-series vector and an event date, it converts the physical date to the event date. The event date becomes zero and all other dates shift accordingly.
}
\usage{
phys2eventtime(z, events, width = 10)
}
\arguments{
  \item{z}{
z is a zoo object containing input data. E.g. this could be all the prices of a bunch of stocks. The column name is the unit name.
events is a data.frame containing 2 columns. The first column ("unit") is the name of the unit. The second column is the date/time ("when") when the event happened.
}
  \item{events}{
A data.frame containing unit name and corresponding event dates.
}
  \item{width}{
Width corresponds to the number of days on each side of the event date.
}
}
\value{
Returns a list containing 1. A zoo object indexed with event time, and having "enough data points" and 2. a vector which describes the status of each unit in the 
original data ( this maybe more than the no of units in 1 ).
}

\author{
Ajay Shah, Ashim Kapoor, Pushpak Sarkar.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (z, events, width = 10) 
{
    events$unit <- as.character(events$unit)
    timeshift <- function(x, when) {
        location <- findInterval(when, index(x))
        if ((location <= 1) | (location >= length(x))) {
            return(list(result = NULL, outcome = "wrongspan"))
        }
        remapped <- zoo(as.numeric(x), order.by = (-location + 
            1):(length(x) - location))
        list(result = remapped, outcome = "success")
    }
    outcomes <- character(nrow(events))
    z.e <- zoo(1, order.by = as.integer(1))
    for (eventnum in 1:nrow(events)) {
        if (!(events$unit[eventnum] \%in\% colnames(z))) {
            outcomes[eventnum] <- "unitmissing"
            next
        }
        attempt <- timeshift(z[, events$unit[eventnum]], events$when[eventnum])
        if (attempt$outcome == "success") {
            z.e <- cbind(z.e, attempt$result)
        }
        outcomes[eventnum] <- attempt$outcome
    }
    outcomes <- outcomes
    z.e <- z.e[, -1]
    colnames(z.e) <- which(outcomes == "success")
    badcolumns <- NULL
    if (width > 0) {
        for (i in 1:ncol(z.e)) {
            tmp <- z.e[, i]
            tmp <- na.locf(tmp, na.rm = FALSE, maxgap = 4)
            tmp <- na.locf(tmp, na.rm = FALSE, maxgap = 4, fromLast = TRUE)
            tmp2 <- window(tmp, start = -width, end = +width)
            if (any(is.na(tmp2))) {
                outcomes[as.numeric(colnames(z.e)[i])] <- "wdatamissing"
                badcolumns <- c(badcolumns, i)
            }
            else {
                z.e[, i] <- tmp
            }
        }
        if (any(outcomes == "wdatamissing")) {
            z.e <- z.e[, -badcolumns]
        }
    }
    stopifnot(sum(outcomes == "success") == NCOL(z.e))
    list(z.e = z.e, outcomes = factor(outcomes))
  }
}
\keyword{ phys2eventime }

