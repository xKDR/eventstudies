eventstudy <- function(firm.returns = NULL,
                       eventList,
                       width = 10,
                       is.levels =  FALSE,
                       type = "marketResidual",
                       to.remap = TRUE,
                       remap = "cumsum",
                       inference = TRUE,
                       inference.strategy = "bootstrap",
                       ...) {
                                        # type = "marketResidual", "excessReturn", "AMM", "None"
  extra.var <- list(...)

  if (type == "None" && !is.null(firm.returns)) {
    outputModel <- firm.returns
  }

  if (is.levels == TRUE) {
    firm.returns <- diff(log(firm.returns)) * 100
  }
  
### Run models
  ## AMM
  if (type == "lmAMM") {
    ## AMM residual to time series
    timeseriesAMM <- function(firm.returns, X, verbose = FALSE, nlags = 1) {
      tmp <- resid(lmAMM(firm.returns = firm.returns,
                         X = X,
                         nlags = nlags,
                         verbose = FALSE))
      tmp.res <- zoo(x = tmp, order.by = as.Date(names(tmp)))
    }
    ## Estimating AMM regressors
    regressors <- makeX(market.returns = extra.var$market.returns,
                        others = extra.var$others,
                        switch.to.innov = extra.var$switch.to.innov,
                        market.returns.purge = extra.var$market.returns.purge,
                        nlags = extra.var$nlags)
    if(NCOL(firm.returns)==1){
      ## One firm
      outputModel <- timeseriesAMM(firm.returns = firm.returns,
                                   X = regressors,
                                   verbose = FALSE,
                                   nlags = 1)
    } else {
      ## More than one firm
                                        # Extracting and merging
      tmp.resid <- sapply(colnames(firm.returns), function(y)
                          {
                            timeseriesAMM(firm.returns = firm.returns[,y],
                                          X = regressors,
                                          verbose = FALSE,
                                          nlags = 1)
                          })
      outputModel <- do.call("merge",tmp.resid)
    }
  } ## end AMM

  ## marketResidual
  if (type == "marketResidual") {
    outputModel <- marketResidual(firm.returns, ...)
  }

  ## excessReturn
  if (type == "excessReturn") {
    outputModel <- excessReturn(firm.returns, ...)
  }
  
### Converting index outputModel to Date
  index(outputModel) <- as.Date(index(outputModel))
  ## Stop if there is only one firm: phys2eventtime breaks down
  if(NCOL(outputModel)==1){stop("Event study does not work for one firm/column")}
    
### Convert to event frame
  es <- phys2eventtime(z=outputModel, events=eventList, width=width)
  es.w <- window(es$z.e, start = -width, end = width)
  ## Adding column names to event output
  cn.names <- eventList[which(es$outcomes=="success"),1]
  if(length(cn.names)==1){
    cat("Event date exists only for",cn.names,"\n")
    inference <- FALSE
    cat("No inference strategy for one column","\n")
  } else {
    colnames(es.w) <- cn.names
  }
  
### Remapping event frame
  if (to.remap == TRUE) {
    es.w <- switch(remap,
                   cumsum = remap.cumsum(es.w, is.pc = FALSE, base = 0),
                   cumprod = remap.cumprod(es.w, is.pc = TRUE,
                     is.returns = TRUE, base = 100),
                   reindex = remap.event.reindex(es.w)
                   )
  }
  
### Inference: confidence intervals
  if(inference == TRUE){
    ## Bootstrap
    if(inference.strategy == "bootstrap"){
      result <- inference.bootstrap(es.w = es.w, to.plot = FALSE)
    }
    ## Wilcoxon
    if(inference.strategy == "wilcoxon"){
      result <- inference.wilcox(es.w = es.w, to.plot = FALSE)
    }
  } else {
    ## Providing event frame as default output
    result <- es.w
  }
  if(to.remap==TRUE){remapping <- remap} else {remapping <- "none"}
    final.result <- list(eventstudy.output=result,
                         outcomes=as.character(es$outcomes),
                         inference=inference.strategy,
                         width=width, remap=remapping)
  class(final.result) <- "es"
  return(final.result)
}

#########################
## Functions for class es
#########################
print.es <- function(x, ...){
  cat("The", x$inference, "inference output for CI and",
      colnames(x$eventstudy.output)[2], "response:", "\n")
  return(x$eventstudy.output)
}

summary.es <- function(object, ...){
  cat("Event study", colnames(object$eventstudy.output)[2], "response with",
      object$inference, "inference for CI:\n")
  print(object$eventstudy.output)
  cat("\n","Event outcome has",length(which(object$outcomes=="success")),
      "successful outcomes out of", length(object$outcomes),"events:","\n")
  object$outcomes
}

plot.es <- function(x, xlab="Event time",
                    ylab="", main="", col="dark slate blue"){
  big <- max(abs(x$eventstudy.output))
  hilo <- c(-big,big)
  width <- (nrow(x$eventstudy.output)-1)/2
  plot(-width:width, x$eventstudy.output[,2], type="l", lwd=2, ylim=hilo,
       col=col,xlab= xlab, ylab = ylab,
       main=paste(main))
  points(-width:width, x$eventstudy.output[,2])
  lines(-width:width, x$eventstudy.output[,"2.5%"],
        lwd=1, lty=2, col=col)
  lines(-width:width, x$eventstudy.output[,"97.5%"],
        lwd=1, lty=2, col=col)
  abline(h=0,v=0)
}
