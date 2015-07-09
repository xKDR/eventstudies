# This does bootstrap inference for the difference in the
# average "car" between t1 and t2 (both in event time).
# es.w is a zoo object, where rows are in event time
# and columns are units of observation.
# Sampling with replacement is done within the units of
# observation. Each time, the Ecar(t1) and Ecar(t2) is
# computed.
# By default, the statistic of interest is the ratio
#  Ecar(t2)/Ecar(t1)
# But if operator="difference" is sent in, then the
# statistic of interest shifts to Ecar(t2)-Ecar(t1).

################
## Bootstrap
################

inference.change.boot <- function(es.w, t1, t2, operator = "ratio",
                                  conf = .95) {
  stopifnot(operator %in% c("ratio","difference"))

  tmp <- t(as.matrix(es.w[c(t1,t2),]))
  if (operator == "ratio") {
    change <- tmp[,2]/tmp[,1]
  }
  if (operator == "difference") {
    change <- tmp[,2]-tmp[,1]
  }

  mymean <- function(x,d) {mean(x[d], na.rm = TRUE)}
  b <- boot(change, mymean, R=1000)
  ci <- boot.ci(b, type = "bca", conf = conf)
  list(est = b$t0, lo = ci$bca[1,4], hi = ci$bca[1,5])
}

# es.w is a zoo object with certain rows (e.g. from -10 to 10)
# that define the event window, and columns with data for units.
# This function does bootstrap inference for the entire
# Ecar, i.e. main graph of the event study.
inference.bootstrap <- function(es.w, to.plot = TRUE,
                                boot.run=1000,
                                xlab = "Event time",
                                ylab = "Cumulative returns of response series",
                                main = "Event study plot") {
  if(NCOL(es.w) == 1){
    stop("More than one series is required for inference.")
  }         
  Ecar <- function(transposed, d) {
    colMeans(transposed[d, , drop=FALSE], na.rm = TRUE)
  }
  tmp <- t(as.matrix(es.w))
  b <- boot(tmp, Ecar, R = boot.run)
  

  result <- NULL
  for (i in 1:ncol(b$t)) {
    result <- rbind(result, quantile(b$t[,i], prob = c(.025,.975),
                                       na.rm = TRUE))
  }
  result <- cbind(result[,1], b$t0, result[,2])
  rownames(result) <- rownames(es.w)
  colnames(result) <- c("2.5%","Mean","97.5%")
  if(to.plot == TRUE){
    plot.inference(result, xlab = "Event time", ylab = ylab,
                   main = "", col = "blue")
  }
  return(result)
}

