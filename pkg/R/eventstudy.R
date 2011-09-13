library(boot)
library(zoo)

# Upon input
#   z is a zoo object containing input data. E.g. this could be all the 
#     prices of a bunch of stocks. The column name is the unit name.
#   events is a data.frame containing 2 columns. The first column
#     ("unit") is the name of the unit. The second column is the date/time
#     ("when") when the event happened.
# For each event, the outcome can be:
#   unitmissing : a unit named in events isn't in z
#   wrongspan : the event date isn't placed within the span of data for the unit
#   wdatamissing: too many NAs within the crucial event window.
#   success : all is well.
# A vector of these outcomes is returned.
phys2eventtime <- function(z, events, width=10) {

  # Just in case events$unit has been sent in as a factor --
  events$unit <- as.character(events$unit)
  events$when <- as.character(events$when)
  # Given a zoo time-series vector x, and an event date "when",
  # try to shift this vector into event time, where the event date
  # becomes 0 and all other dates shift correspondingly.
  # If this can't be done, then send back NULL with an error code.
  timeshift <- function(x, when) {
    location <- findInterval(when, index(x))
    if ((location <= 1) | (location >= length(x))) {
      return(list(result=NULL, outcome="wrongspan"))
    }
    remapped <- zoo(as.numeric(x), order.by=(-location+1):(length(x)-location))
    list(result=remapped, outcome="success")
  }

  # Main loop to build up a data object in event time --
  outcomes <- character(nrow(events))
  z.e <- zoo(1, order.by=as.integer(1)) # zoo::cbind() requires initialising z.e
  for (eventnum in 1:nrow(events)) {
    if (!(events$unit[eventnum] %in% colnames(z))) {
      outcomes[eventnum] <- "unitmissing"
      next
    }
    attempt <- timeshift(z[,events$unit[eventnum]], events$when[eventnum])
    if (attempt$outcome=="success") {
      z.e <- cbind(z.e, attempt$result)
    }
    outcomes[eventnum] <- attempt$outcome
  }
  outcomes <- outcomes
  z.e <- z.e[,-1, drop = FALSE]                       #get rid of that junk initialisation
  colnames(z.e) <- which(outcomes=="success")

  ## Now worry about whether there's information within the event window
  ## (This entire cleaning+checking can be switched off by specifying width=0)
  badcolumns <- NULL
  if (width > 0) {
    for (i in 1:ncol(z.e)) {
      tmp <- z.e[,i]
      tmp <- na.locf(tmp, na.rm=FALSE, maxgap=4)
      tmp <- na.locf(tmp, na.rm=FALSE, maxgap=4, fromLast=TRUE)
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

# A function which consumes a zoo object where there are lots of events
# (as columns)
# The contents are all levels
# For each column, the event date value is set to 100 and all other
# values are scaled accordingly.
remap.event.reindex <- function(z) {
  eventvals <- as.numeric(window(z, start=0, end=0))
  for (i in 1:ncol(z)) {
    z[,i] <- 100*z[,i]/eventvals[i]
  }
  z
}

# If is.pc then a value like "1" means 0.01
remap.cumsum <- function(z, is.pc=TRUE, base=0) {
  for (i in 1:ncol(z)) {
    tmp <- z[,i]
    if (is.pc) {
      tmp <- tmp/100
    }
    z[,i] <- base+cumsum(tmp)
  }
  z
}

# is.pc and is.returns are TRUE
#    values are like "1" for 1%
# is.returns is true but not is.pc
#    values are like "0.01" for 1%
# is.returns is false                         in this case is.pc is ignored!
#    values are like 1.01 for 1%
remap.cumprod <- function(z, is.pc=TRUE, is.returns=TRUE, base=100) {
  for (i in 1:ncol(z)) {
    tmp <- z[,i]
    if (is.returns) {
      if (is.pc) {
        tmp <- tmp/100
      }
      tmp <- 1+tmp
    }
    tmp[1] <- base
    z[,i] <- cumprod(tmp)
  }
}

# This does bootstrap inference for the difference in the
# average "car" between t1 and t2 (both in event time).
# z.e is a zoo object, where rows are in event time
# and columns are units of observation.
# Sampling with replacement is done within the units of
# observation. Each time, the Ecar(t1) and Ecar(t2) is
# computed.
# By default, the statistic of interest is the ratio
#  Ecar(t2)/Ecar(t1)
# But if operator="difference" is sent in, then the
# statistic of interest shifts to Ecar(t2)-Ecar(t1).
inference.change.boot <- function(z.e, t1, t2, operator="ratio", conf=.95) {
  stopifnot(operator %in% c("ratio","difference"))

  tmp <- t(as.matrix(z.e[c(t1,t2),]))
  if (operator=="ratio") {
    change <- tmp[,2]/tmp[,1]
  }
  if (operator=="difference") {
    change <- tmp[,2]-tmp[,1]
  }

  mymean <- function(x,d) {mean(x[d], na.rm=TRUE)}
  b <- boot(change, mymean, R=1000)
  ci <- boot.ci(b, type="bca", conf=conf)
  list(est=b$t0, lo=ci$bca[1,4], hi=ci$bca[1,5])
}

# z.e is a zoo object with certain rows (e.g. from -10 to 10)
# that define the event window, and columns with data for units.
# This function does bootstrap inference for the entire
# Ecar, i.e. main graph of the event study.
inference.Ecar <- function(z.e) {
  Ecar <- function(transposed, d) {
    colMeans(transposed[d,], na.rm=TRUE)
  }
  tmp <- t(as.matrix(z.e))
  b <- boot(tmp, Ecar, R=1000)

  results <- NULL
  for (i in 1:ncol(b$t)) {
    results <- rbind(results, quantile(b$t[,i], prob=c(.025,.975)))
  }
  results <- cbind(results[,1], b$t0, results[,2])
  rownames(results) <- rownames(z.e)
  colnames(results) <- c("2.5%","Mean","97.5%")
  results
}
