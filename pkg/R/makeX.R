
###############################################################
# A function that calls do.one.piece, and works through several
# different periods to provide the right RHS matrix.
###############################################################
source('AMM.R')
makeX <- function(rM1, others,
                  switch.to.innov=rep(TRUE, NCOL(others)),
                  rM1purge=TRUE,
                  nlags=5,
                  dates=NULL,
                  verbose=TRUE) {
  if (verbose) {cat("0. Checking args\n")}
  stopifnot(all.equal(index(rM1), index(others)),
            length(switch.to.innov)==NCOL(others))
  if (!is.null(dates)) {
    stopifnot(class(dates) == "Date")
  }
  if (verbose) {cat("1. Checking dates.\n")}
  if (is.null(dates)) {
    dates <- c(start(rM1),end(rM1))
  }
  if(head(dates,1)!=head(index(rM1),1)){
    stop("Start date provided and the start date of the dataset do not match \n")
  }
  if(tail(dates,1)!=tail(index(rM1),1)){
    stop("End date provided and the end date of the dataset do not match \n")
  }
  if (verbose) {cat("2. Run through all the pieces --\n")}
  for (i in 1:(length(dates)-1)) {
    t1 <- dates[i]
    t2 <- dates[i+1]
    if (i != (length(dates)-1)) {t2 <- t2 -1}
    if (verbose) {
      cat("   Focusing down from date = ", as.character(t1), " to ", as.character(t2), "\n")
    }
    tmp.rM1 <- window(rM1, start=t1, end=t2)
    tmp.others <- window(others, start=t1, end=t2)
    a <- do.one.piece(tmp.rM1, tmp.others, switch.to.innov, rM1purge, nlags, verbose)
    if (i > 1) {
      res.rM1 <- c(res.rM1, a$rM1.purged)
      res.innov <- rbind(res.innov, a$innov)
    } else {
      res.rM1 <- a$rM1.purged
      res.innov <- a$innov
    }
  }
  if (verbose) {cat("2. Make a clean X and send it back --\n")}
  X <- cbind(res.rM1, res.innov)
  if (NCOL(res.innov) == 1) {colnames(X) <- c("rM1","z")}
  else {colnames(X) <- c("rM1", colnames(res.innov))}
  X
}

