eventstudy <- function(firm.returns = NULL,
                       eventList,
                       width = 10,
                       is.levels =  FALSE,
                       type = "marketResidual",
                       to.remap = TRUE,
                       remap = "cumsum",
                       inference = TRUE,
                       inference.strategy = "bootstrap",
                       to.plot = TRUE,
                       xlab = "Event time",
                       ylab = "Cumulative returns of response series",
                       main = "Event study plot",
                       ...) {
                                        # type = "marketResidual", "excessReturn", "AMM", "None"
  if (type == "None" && !is.null(firm.returns)) {
    outputModel <- firm.returns
  }

  if (is.levels == TRUE) {
    firm.returns <- diff(log(firm.returns)) * 100
  }

### Run models
  ## AMM
  if (type == "AMM") {
    ## AMM residual to time series
    timeseriesAMM <- function(firm.returns,X,verbose=FALSE,nlags=1){
      tmp <- resid(lmAMM(firm.returns,X,nlags))
      tmp.res <- zoo(tmp,as.Date(names(tmp)))
    }    
    if(NCOL(firm.returns)==1){
      ## One firm
      outputModel <- timeseriesAMM(firm.returns=StockPriceReturns[,1], 
                                   X=regressors, verbose=FALSE, nlags=1)
    } else {
      ## More than one firm
                                        # Extracting and merging
      tmp.resid <- sapply(colnames(StockPriceReturns)[1:3],function(y)
                          timeseriesAMM(firm.returns=StockPriceReturns[,y],
                                        X=regressors,
                                        verbose=FALSE,
                                        nlags=1))
      outputModel <- do.call("merge",tmp.resid)
    }    
  }

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
      result <- inference.bootstrap(es.w = es.w, to.plot = to.plot, xlab = xlab,
                                    ylab = ylab, main = main)
    }
    ## Wilcoxon
    if(inference.strategy == "wilcoxon"){
      result <- inference.wilcox(es.w = es.w, to.plot = to.plot, xlab = xlab,
                                 ylab = ylab, main = main)
    }
  } else {
    ## Providing event frame as default output
    result <- es.w
  }
    final.result <- list(eventstudy.output=result,
                         outcomes=as.character(es$outcomes),
                         inference=inference.strategy, nlags=nlags)
  class(final.result) <- "es"
  return(final.result)
}

#########################
## Functions for class es
#########################
print.es <- function(){


}

summary.es <- function(){


}

plot.es <- function(){


}
