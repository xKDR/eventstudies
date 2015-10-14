## The following code plots the result obtained from the eventstudy
## wrapper. "plot.inference" plots the estimate of impact of event
## along with confidence intervals for each of the 3 strategies.
## "plot.simple" plots the CAR when inference is FALSE

plot.inference <- function(inference, ...){

  big1 <- max(apply(inference, 1,
                    function(y) max(abs(y[is.finite(y)]))))
  hilo1 <- c(-big1, big1)
  width <- NROW(inference) / 2
  plot.simple(inference[ , 2], ylim = hilo1, ...)
  lines((-width + 1):width, inference[ , "2.5%"], lwd = 1,
        lty = 2, ...)
  lines((-width + 1):width, inference[ , "97.5%"], lwd = 1,
        lty = 2, ...)
}

plot.simple <- function(x, ...){
  width <- NROW(x) / 2
  plot.default((-width + 1):width, x, type = "l", lwd = 2, ...)
  points((-width + 1):width, x)
  abline(h = 0,v = 0)
}
