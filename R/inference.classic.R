# This does classical inference using t-statistic for the average
# "car" across time period before and after the event. 
# es.w is a zoo object, where rows are in event time
# and columns are units of observation. T-stat values along with
# confidence intervals are computed for each observation in time. 

####################
## Classical t-test
####################
inference.classic <- function(es.w, to.plot = TRUE,
                              xlab = "Event time",
                              ylab = "Cumulative returns of response series",
                              main = "Event study plot"){
  if(NCOL(es.w) == 1){
    stop("More than one series is required for inference.")
  }         
  ## Confidence interval and mean estimate for t-stat 
  CI <- t(apply(es.w, 1, function(x)
                res <- t.test(x = x, y = NULL,
                              alternative = "two.sided",
                              mu = 0, conf.level = 0.95)))
  CI <- t(sapply(1: length(CI), function(x)
               CI[[x]]$conf.int))
  Mean <- apply(es.w, 1, mean, na.rm = TRUE)
  result <- cbind(CI[, 1], Mean, CI[, 2])
  colnames(result) <- c("2.5%","Mean","97.5%")
  rownames(result) <- rownames(Mean)
  if(to.plot == TRUE){
    plot.inference(result, xlab = "Event time", ylab = ylab,
                   main = "", col = "blue")
  }
  return(result)  
}

