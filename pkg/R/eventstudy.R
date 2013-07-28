eventstudy <- function(inputData = NULL,
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
  if (type == "None" && !is.null(inputData)) {
    outputModel <- inputData
  }

  ## else {
  ##   stop("inputData or \"None\" type missing")
  ## }

  if (is.levels == TRUE) {
    inputData <- diff(log(inputData)) * 100
  }

### Run models
  ## AMM
  if (type == "AMM") {
    if(amm.type == "onefirm"){
      tmp.outputModel <- AMM(rj = inputData, ...)
      outputModel <- zoo(tmp.outputModel$residual,index(tmp.outputModel))
    }
    if(amm.type == "manyfirms"){
      tmp.outputModel <- AMM(regressand = inputData, ...)
      outputModel <- zoo(tmp.outputModel$residual,index(tmp.outputModel))
    }
    if(amm.type == "firmExposures"){
      stop("amm.type firmExposures not used for event study analysis")
    }
    
  }

  ## marketResidual
  if (type == "marketResidual") {
    outputModel <- marketResidual(data.object = inputData, ...)
  }

  ## excessReturn
  if (type == "excessReturn") {
    outputModel <- excessReturn(data.object = inputData, ...)
  }

### Convert to event frame
  es <- phys2eventtime(z=outputModel, events=eventList, width=width)
  colnames(es) <- eventList[which(es$outcomes=="success"),1]
  es.w <- window(es$z.e, start = -width, end = width)
  
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
    
  return(result)
}
