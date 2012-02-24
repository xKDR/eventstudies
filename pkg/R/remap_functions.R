library(boot)
library(zoo)

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

