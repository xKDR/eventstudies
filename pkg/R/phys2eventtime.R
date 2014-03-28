# Upon input
#   z is a zoo object containing input data. E.g. this could be all the 
#     prices of a bunch of stocks. The column name is the unit name.
#   events is a data.frame containing 2 columns. The first column
#     ("outcome.unit") is the name of the unit. The second column is the date/time
#     ("event.when") when the event happened.
# For each event, the outcome can be:
#   unitmissing : a unit named in events isn't in z
#   wrongspan : the event date isn't placed within the span of data for the unit
#   wdatamissing: too many NAs within the crucial event window.
#   success : all is well.
# A vector of these outcomes is returned.
phys2eventtime <- function(z, events, width=10) {
  # Just in case events$outcome.unit has been sent in as a factor --
  events$outcome.unit <- as.character(events$outcome.unit)
  if(is.factor(events$event.when)) stop("Sorry you provided a factor as an index")
  # Given a zoo time-series z, and an event date "event.when",
  # try to shift this vector into event time, where the event date
  # becomes 0 and all other dates shift correspondingly.
  # If this can't be done, then send back NULL with an error code.
  ## takes the event list as an argument and uses already existing
  ## time-series variable z

                                        # check the dimensions of "z""
  if (is.null(ncol(z))) {
    stop(paste(deparse("z"), "should be a zoo series with at least one column."))
  }

  timeshift <- function(x) {
    firm.present <- match(x[1], colnames(z), nomatch = -1) != -1
    if (!firm.present) {
      return(list(result=NULL, outcome="unitmissing"))
    }
                                        # take the previous date if
                                        # the exact event date is not
                                        # found
    location <- findInterval(as.Date(x[2]), index(z[, x[1]]))
    if ((location <= 1) | (location >= length(index(z)))) {
      return(list(result=NULL, outcome="wrongspan"))
    }
    remapped <- zoo(as.numeric(z[,x[1]]), order.by=(-location+1):(length(z[,x[1]])-location))
    return(list(result=remapped, outcome="success"))
  }
  
  answer <- apply(events, 1, timeshift)
  answer <- unlist(answer, recursive = FALSE)
  rownums <- grep("outcome", names(answer))
  outcomes <- as.character(do.call("c", answer[rownums]))
  z.e <- do.call("cbind", answer[rownums[which(answer[rownums] == "success")] - 1])

  if (length(z.e) == 0) {               # no point of going forward
    return(list(z.e = z.e, outcomes = factor(outcomes)))
  }

  colnames(z.e) <- which(outcomes == "success")

  ## Now worry about whether there's information within the event window
  ## (This entire cleaning+checking can be switched off by specifying width=0)
  badcolumns <- NULL
  if (width > 0) {
    for (i in 1:ncol(z.e)) {
      tmp <- z.e[,i]
      tmp2 <- window(tmp, start=-width, end=+width)
      if (any(is.na(tmp2))) {
        outcomes[as.numeric(colnames(z.e)[i])] <- "wdatamissing"
        badcolumns <- c(badcolumns, i)
      } else {
        z.e[,i] <- tmp                # Put the fixed up column back in.
      }
    }
    if (any(outcomes == "wdatamissing")) {
      z.e <- z.e[, -badcolumns]
    }
  }
  # Check that we're okay
  stopifnot(sum(outcomes=="success") == NCOL(z.e))
  list(z.e=z.e, outcomes=factor(outcomes))
}
