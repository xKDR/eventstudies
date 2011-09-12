library(zoo)
library(foreach)
library(doMC)
library(multicore)
registerDoMC()

# -------------------------------------------------------------------------
# Functions to run this simulation exercise

# Creating random prices data
create.data <- function(ncol=1000,nrow=1000){
  len <- ncol*nrow
  r <- matrix(rnorm(len), nrow=nrow)
  p0 <- runif(ncol, min=10, max=200)
  p <- matrix(NA, nrow=nrow, ncol=ncol)
  for (i in 1:1000) {
    p[,i] <- cumprod(c(p0[i], 1+(r[,i]/100)))[-1]
  }
  p <- zoo(p, order.by=seq(as.Date("2004-01-01"), by=1,
                length.out=1000))
  colnames(p) <- paste("a",1:ncol,sep=".")
  p
}

# Sampling a random set of firms and event dates
sample.size <- function(data,N=100){
  firm.ids <- sample(colnames(data), size=N, replace=FALSE)
  firm.dates <- index(data)[sample(1:1000, size=N, replace=FALSE)]
  list(firm.ids=firm.ids,firm.dates=firm.dates)
}

# Use event studies package to obtain bootstrap results
package.es <- function(data,firm.ids,firm.dates,width=10){
  library(eventstudies)
  library(boot) # This is a bug! If eventstudies depends on boot,
                # why isn't it auto loaded? 
  z <- data[,firm.ids]
  z <- diff(log(z))
  events <- data.frame(unit=colnames(z), when=firm.dates)
  es.results <- phys2eventtime(z, events, width=0)
  es.w <- window(es.results$z.e, start=-width, end=+width)
  es.w <- remap.cumsum(es.w, is.pc=FALSE, base=0)
  inference.Ecar(es.w)
}

plot.es <- function(data,width=10){
  hilo <- range(data)
  plot(-width:width, data[,2], type="l", lwd=2, ylim=hilo, col="blue",
       xlab="Event time (days)", ylab="(Cum.) bps of returns",
       main="Event Study")
  points(-width:width, data[,2])
  lines(-width:width, data[,1], lwd=2, lty=2, col="blue")
  lines(-width:width, data[,3], lwd=2, lty=2, col="blue")
  abline(h=0,v=0)
}
# These two functions tests for significance only on incidence
measure.size <- function(data,width=10){
  ans <- data[,1]*data[,3] < 0
  TrueReject <- ans[width+1]==TRUE
  TrueReject
}

measure.magnitude <- function(data,width=10){
  dat <- data[width+1,]
  locate.sig <- as.numeric(dat[1])*as.numeric(dat[3])> 0
  if(locate.sig==TRUE){
    magnitude <- as.numeric(dat[2])
    magnitude
  }else{
    magnitude <- 0
    magnitude
  }
}

# This is testing across the entire window -- this is important
# particularly in the context of event studies. Two functions
# below are modifications of two measure functions above.  
measure.size.general <- function(data){
  ans <- data[,1]*data[,3] < 0
  TrueReject <- sum(ans)==length(ans)
  TrueReject
}

measure.magnitude.general <- function(data){
  locate.sig <- which(data[,1]*data[,3] > 0) # AT t0, this should
  # kick in and show positive results at that time stamp
  if(length(locate.sig)==0){
    shock <- 0
    shock
  }else{
    shock <- mean(data[locate.sig,])
    shock
  }
}

induce.shock <- function(data,firm.ids,firm.dates,event.shock){
  for(i in 1:length(firm.ids)){
    rownames <- index(p) >= firm.dates[i]
    data[rownames,i] <- data[rownames,i]*event.shock
  }
  data
}
# ---------------------------------------------------------------------------     
# Test 1 : Measuring the size of a test.
# You can vary:
# N the number of runs
# sample size
# and the window length.

# Here, in truth, H0 (event is not important) is TRUE.
# So when this is run 100 times, H0 should be falsely rejected 5% of the time.
# The size of the test should be alpha (in this case 5%). 
# If this happens, the size of the test is correct.
#
# This should happen correctly for lots of N and lots of window size
# (of course, becoming more sharp as N gets bigger).

# Create the data set
p <- create.data(ncol=1000,nrow=1000)

# Variation one: Number of runs
opt1 <- foreach(i=1:10000,.combine="c") %dopar% {
  cat("Working on run number",i,"\n") 
  dat <- sample.size(data=p,N=100)
  firm.ids <- dat$firm.ids
  firm.dates <- dat$firm.dates
  es.timeframe <- package.es(data=p,firm.ids=firm.ids,firm.dates=firm.dates,width=10)
  measure.size(data=es.timeframe,width=10) 
}

table(opt1)

# Variation two: Vary sample size in each run
opt2 <- foreach(i=1:100,.combine="cbind") %:% foreach(j=100:250, .combine="c")  %dopar% {
  cat("Working on sample size of",j,"\n") 
  dat <- sample.size(data=p,N=j)
  firm.ids <- dat$firm.ids
  firm.dates <- dat$firm.dates
  es.timeframe <- package.es(data=p,firm.ids=firm.ids,firm.dates=firm.dates,width=10)
  measure.size(data=es.timeframe,width=10) 
}

rowSums(opt2)
colSums(opt2)

# Variation three: Vary the window size
opt3 <- foreach(i=1:100,.combine="cbind") %:% foreach(j=10:100,.combine="c")  %dopar% {
  cat("Working on window length of",j,"\n") 
  dat <- sample.size(data=p,N=100)
  firm.ids <- dat$firm.ids
  firm.dates <- dat$firm.dates
  es.timeframe <- package.es(data=p,firm.ids=firm.ids,firm.dates=firm.dates,width=j)
  measure.size(data=es.timeframe,width=j) 
}

rowSums(opt3)
colSums(opt3)

# Variation four: Vary the sample size and the window size
opt4 <- foreach(i=100:250,.combine="cbind") %:% foreach(j=10:100,.combine="c") %dopar% {
  cat("Working on width of",j,"and sample size of",i,"\n")
  dat <- sample.size(data=p,N=i)
  firm.ids <- dat$firm.ids
  firm.dates <- dat$firm.dates
  es.timeframe <- package.es(data=p,firm.ids=firm.ids,firm.dates=firm.dates,width=j)
  measure.size(data=es.timeframe,width=j) 
}

rowSums(opt4)
colSums(opt4)

# ---------------------------------------------------------------------------
# Small event shocks should not get discerned
# But as the number of events goes up, even small shocks should get
#  discerned
# Our discerning the event should happen regardless of where the shock
#  kicks in, as long as it's within the event window.
  
# Power of the test
power1 <- foreach(i=1:2,.combine="cbind") %:% foreach(j=99:100,.combine="c")  %dopar% {
  cat("Working with a shock of",j,"% \n") 
  dat <- sample.size(data=p,N=100)
  firm.ids <- dat$firm.ids
  firm.dates <- dat$firm.dates
  k <- induce.shock(data=p,firm.ids=firm.ids,firm.dates=firm.dates,event.shock=(1+(j/100)))
  es.timeframe <-  package.es(data=k,firm.ids=firm.ids,firm.dates=firm.dates,width=10)
  tmp <- measure.magnitude(data=es.timeframe)
  measure.magnitude(data=es.timeframe,width=10)
}

# Changing Sample size along with shock magnitude
power2 <- foreach(i=100:250,.combine="cbind") %:%  foreach(j=1:20,.combine="c") %dopar%{
  p <- create.data(ncol=1000,nrow=1000)
  cat("Working with a shock of",j,"% \n") 
  dat <- sample.size(data=p,N=i)
  firm.ids <- dat$firm.ids
  firm.dates <- dat$firm.dates
  k <- induce.shock(data=p,firm.ids=firm.ids,firm.dates=firm.dates,event.shock=(1+(j/100)))
  es.timeframe <-  package.es(data=k,firm.ids=firm.ids,firm.dates=firm.dates,width=10)
  measure.magnitude(data=es.timeframe)
}

  # Changing width along with shock magnitude
power3 <- foreach(i=10:100,.combine="cbind") %:%  foreach(j=1:20,.combine="c") %dopar%{
  p <- create.data(ncol=1000,nrow=1000)
  cat("Working with a shock of",j,"% \n") 
  dat <- sample.size(data=p,N=100)
  firm.ids <- dat$firm.ids
  firm.dates <- dat$firm.dates
  k <- induce.shock(data=p,firm.ids=firm.ids,firm.dates=firm.dates,event.shock=(1+(j/100)))
  es.timeframe <-  package.es(data=k,firm.ids=firm.ids,firm.dates=firm.dates,width=i)
  measure.magnitude(data=es.timeframe)
}

save(opt1,opt2,opt3,opt4,power1,power2,power3,file="results.rda")
  
# ---------------------------------------------------------------------------

## # One more frontier: move the event date to x days after the
## # actual event date.
## # (but within the window).

# ---------------------------------------------------------------------------

### How about false rejection within the window with no change in
### the actual incidence of event? This seems to be a problem with
### the inference procedure as of now.






