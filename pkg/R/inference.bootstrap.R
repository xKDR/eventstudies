library(boot)
library(zoo)


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

# Plotting inference
plotInference <- function(inference, xlab, ylab, main){
  big <- max(abs(inference))
  hilo <- c(-big,big)
  width <- (nrow(inference)-1)/2
  plot(-width:width, inference[,"Mean"], type="l", lwd=2, ylim=hilo,
       col="dark slate blue",
       xlab= xlab, ylab = ylab,
       main=paste(main))
  points(-width:width, inference[,"Mean"])
  lines(-width:width, inference[,"2.5%"], lwd=1, lty=2, col="dark slate blue")
  lines(-width:width, inference[,"97.5%"], lwd=1, lty=2, col="dark slate blue")
  abline(h=0,v=0)
}

# z.e is a zoo object with certain rows (e.g. from -10 to 10)
# that define the event window, and columns with data for units.
# This function does bootstrap inference for the entire
# Ecar, i.e. main graph of the event study.
inference.bootstrap <- function(z.e,to.plot=FALSE,
                                xlab = "Event time",
                                ylab = "Cumulative returns of response series",
                                main = "Eventstudy plot") {
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
  if(to.plot==TRUE){
    plotInference(inference=results, xlab, ylab, main)
  }
  return(results)
}

#####################
## Wilcoxon sign test
#####################
inference.wilcox <- function(es.w, to.plot = TRUE, xlab = "Event time",
                      ylab = "Cumulative returns of response series",
                      main = "Eventstudy plot"
                      ){
  wx.res <- apply(es.w,1,function(x)
                  res <- wilcox.exact(x, alternative = "two.sided",
                                      conf.int = TRUE,
                                      conf.level = 0.95)["conf.int"])
  list <- unlist(wx.res, recursive = FALSE)
  CI <- do.call(rbind, list)
  Mean <- apply(es.w,1,mean,na.rm=TRUE)
  result <- cbind(CI[,1], Mean, CI[,2])
  colnames(result) <- c("2.5%","Mean","97.5%")
  rownames(result) <- rownames(Mean)
  if(to.plot = TRUE){
    plotInference(inference = result, xlab, ylab, main)
  }
  return(result)
}
