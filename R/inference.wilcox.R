# This does inference using wilcox sign test for the average "car"
# for each observation in time before and after the event. es.w is
# a zoo object, where rows are in event time and columns are units of
# observation.

#####################
## Wilcoxon sign test
#####################
inference.wilcox <- function(es.w, to.plot = TRUE,
                             xlab = "Event time",
                             ylab = "Cumulative returns of response series",
                             main = "Event study plot"
                             ){
  if(NCOL(es.w) == 1){
    stop("More than one series is required for inference.")
  }         
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
  CI <- t(apply(es.w, 1, function(x)
                res <- wilcox.sign.test(x, prob = 0.975)))
  Median <- apply(es.w, 1, median, na.rm = TRUE)
  result <- cbind(CI[, 1], Median, CI[, 2])
  colnames(result) <- c("2.5%", "Median", "97.5%")
  rownames(result) <- rownames(Median)
  if(to.plot == TRUE){
    plot.inference(result, xlab = "Event time",
                   ylab = " ", main = "", col = "blue")
    
  }
  return(result)  
}
