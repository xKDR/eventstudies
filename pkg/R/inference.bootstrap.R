library(boot)
library(zoo)

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
inference.change.boot <- function(es.w, t1, t2, operator="ratio", conf=.95) {
  stopifnot(operator %in% c("ratio","difference"))

  tmp <- t(as.matrix(es.w[c(t1,t2),]))
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
plot.inference <- function(inference, xlab="Event time",
                    ylab="", main="", col.es="dark slate blue"){
  big <- max(abs(es.object$eventstudy.output))
  hilo <- c(-big,big)
  width <- (nrow(es.object$eventstudy.output)-1)/2
  plot(-width:width, es.object$eventstudy.output[,2], type="l", lwd=2, ylim=hilo,
       col=col.es,xlab= xlab, ylab = ylab,
       main=paste(main))
  points(-width:width, es.object$eventstudy.output[,2])
  lines(-width:width, es.object$eventstudy.output[,"2.5%"],
        lwd=1, lty=2, col=col.es)
  lines(-width:width, es.object$eventstudy.output[,"97.5%"],
        lwd=1, lty=2, col=col.es)
  abline(h=0,v=0)
}

# es.w is a zoo object with certain rows (e.g. from -10 to 10)
# that define the event window, and columns with data for units.
# This function does bootstrap inference for the entire
# Ecar, i.e. main graph of the event study.
inference.bootstrap <- function(es.w, to.plot=TRUE,
                                xlab = "Event time",
                                ylab = "Cumulative returns of response series",
                                main = "Event study plot") {
  Ecar <- function(transposed, d) {
    colMeans(transposed[d,], na.rm=TRUE)
  }
  tmp <- t(as.matrix(es.w))
  b <- boot(tmp, Ecar, R=1000)

  results <- NULL
  for (i in 1:ncol(b$t)) {
    results <- rbind(results, quantile(b$t[,i], prob=c(.025,.975)))
  }
  results <- cbind(results[,1], b$t0, results[,2])
  rownames(results) <- rownames(es.w)
  colnames(results) <- c("2.5%","Mean","97.5%")
  if(to.plot==TRUE){
    plot.es(inference=results, xlab, ylab, main)
  }
  return(results)
}

#####################
## Wilcoxon sign test
#####################
inference.wilcox <- function(es.w, to.plot = TRUE, xlab = "Event time",
                      ylab = "Cumulative returns of response series",
                      main = "Event study plot"
                      ){
  ## Wilcoxon sign test
  wilcox.sign.test <- function(x, prob){
    n <- length(x)
    m <- n * (n + 1) / 2
    k <- 1:(m / 2)
    conf.lev <- 1 - 2 * psignrank(k, n)
    no <- round(conf.lev[conf.lev>=prob], 4)
    no.f <- length(no)
    w <- outer(x, x, "+") / 2
    w <- w[lower.tri(w, diag = TRUE)]
    w <- sort(w)
    CI <- c(w[no.f + 1], w[m - no.f])
    prob.val <- 1 - 2 * psignrank(no.f, n)
    return(CI)
  }
  ## Extracting confidence interval 
  CI <- t(apply(es.w,1,function(x)
                res <- wilcox.sign.test(x, prob=0.975)))
  Median <- apply(es.w,1,median,na.rm=TRUE)
  result <- cbind(CI[,1], Median, CI[,2])
  colnames(result) <- c("2.5%","Median","97.5%")
  rownames(result) <- rownames(Median)
  if(to.plot == TRUE){
    plot.inference(inference = result, xlab, ylab, main)
  }
  return(result)  
}



