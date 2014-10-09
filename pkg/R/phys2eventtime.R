# Upon input   
#   z is a zoo object containing input data. E.g. this could be all the 
#     prices of a bunch of stocks. The column name is the unit name.
#   events is a data.frame containing 2 columns. The first column
#     ("name") is the name of the unit. The second column is the date/time
#     ("when") when the event happened.
# For each event, the outcome can be:
#   unitmissing : a unit named in events isn't in z
#   wrongspan : the event date isn't placed within the span of data for the unit
#   wdatamissing: too many NAs within the crucial event window.
#   success : all is well.
# A vector of these outcomes is returned.

phys2eventtime <- function(z, events, width=10) {
  if (is.null(ncol(z))) {
    stop(paste("'z' should be of class zoo/xts with at least one column. Use '[' with drop = FALSE"))
  }
  if (!any(class(events$when) %in% c("POSIXt", "Date"))) {
      stop("events$when should be one of 'Date' or 'date-time' classes.")
  }

  answer <- lapply(1:nrow(events), function(i) timeshift(events[i, ], z))
  outcomes <- sapply(answer, function(x) x$outcome)
  z.e <- do.call(cbind, lapply(answer[outcomes == "success"], function(x) x$result))

  ## If no successful outcome, return NULL to z.e. 
  if (length(z.e) == 0) {               
    return(list(z.e = NULL, outcomes = factor(outcomes)))
  }

  colnames(z.e) <- which(outcomes == "success")

  ## Information verification within 'width'
  ##   :: Will not be executed with width = 0
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
  ## Double check
  stopifnot(sum(outcomes=="success") == NCOL(z.e))
  list(z.e=z.e, outcomes=factor(outcomes))
}

timeshift <- function(x, z) {
  firm.present <- x[, "name"] %in% colnames(z)
  if (!firm.present) {
    return(list(result=NULL, outcome="unitmissing"))
  }

  ## Take previous date if exact data is not found.
  location <- findInterval(x[, "when"], index(z[, x[, "name"]]))
  if ((location <= 1) | (location >= length(index(z)))) {
    return(list(result=NULL, outcome="wrongspan"))
  }

  remapped <- zoo(as.numeric(z[, x[, "name"]]),
                  order.by = (-location + 1):(length(z[, x[, "name"]]) - location))
  return(list(result = remapped, outcome = "success"))
}
