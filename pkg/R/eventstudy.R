eventstudy <- function(firm.returns,
                       eventList,
                       event.window = 10,
                       is.levels =  FALSE,
                       type = "marketResidual",
                       to.remap = TRUE,
                       remap = "cumsum",
                       inference = TRUE,
                       inference.strategy = "bootstrap",
                       model.args = NULL) {
  
  if (type == "None" && !is.null(firm.returns)) {
    outputModel <- firm.returns
    if (length(model.args) != 0) {
        warning(deparse("type"), " = ", deparse("None"),
                " does not take extra arguments, ignoring them.")
    }
  }

  if (type != "None" && is.null(model.args)) {
      stop("modelArgs cannot be NULL when type is not None.")
  }

  if (is.levels == TRUE) {
    firm.returns <- diff(log(firm.returns)) * 100
  }

  ## handle single series
  if (is.null(ncol(firm.returns))) {
      stop("firm.returns should be a zoo series with at least one column. Use '[' with 'drop = FALSE'.")
  }
  firmNames <- colnames(firm.returns)

### Run models
  ## AMM
  if (type == "lmAMM") {

    ## Estimating AMM regressors
    args.makeX <- model.args[names(model.args) %in% formalArgs(makeX)]
    if (!is.null(model.args$nlag.makeX)) {
        args.makeX$nlags <- model.args$nlag.makeX
    }
    regressors <- do.call(makeX, args.makeX)

    args.lmAMM <- model.args[names(model.args) %in% formalArgs(lmAMM)]
    args.lmAMM$X <- regressors

    if (!is.null(model.args$nlag.lmAMM)) {
        args.lmAMM$nlags <- model.args$nlag.lmAMM
    }

    if(NCOL(firm.returns)==1){
      ## One firm
      args.lmAMM$firm.returns <- firm.returns
      tmp <- resid(do.call(lmAMM, args.lmAMM))
      if (is.null(tmp)) {
          cat("lmAMM() returned NULL\n")
          return(NULL)
      }
      outputModel <- zoo(x = tmp, order.by = as.Date(names(tmp)))

    } else {
      ## More than one firm
                                        # Extracting and merging
      tmp.resid <- lapply(colnames(firm.returns), function(y)
                          {
                            args.lmAMM$firm.returns <- firm.returns[, y]
                            tmp <- resid(do.call(lmAMM, args.lmAMM))
                            if (is.null(tmp)) {
                                cat("lmAMM() returned NULL\n")
                                return(NULL)
                            }
                            return(zoo(x = tmp, order.by = as.Date(names(tmp))))
                        })
      names(tmp.resid) <- colnames(firm.returns)
      outputModel <- do.call(merge.zoo, tmp.resid)
    }
  } ## end AMM

  ## marketResidual
  if (type == "marketResidual") {
    outputModel <- marketResidual(firm.returns, model.args$market.returns)
  }

  ## excessReturn
  if (type == "excessReturn") {
    outputModel <- excessReturn(firm.returns, model.args$market.returns)
  }
  
### Converting index outputModel to Date
  index(outputModel) <- as.Date(index(outputModel))
    
### Convert to event frame
  ## change the dimensions if there is only one firm
  if (is.null(ncol(outputModel))) {
      attr(outputModel, "dim") <- c(length(outputModel), 1)
      attr(outputModel, "dimnames") <- list(NULL, firmNames)
      colnames(outputModel) <- firmNames
  }

  es <- phys2eventtime(z = outputModel, events=eventList, width=0)

  if (is.null(es$z.e) || length(es$z.e) == 0) {
    es.w <- NULL
    cn.names <- character(length = 0)
  } else {
    es.w <- window(es$z.e, start = -event.window, end = event.window)
                                        # Adding column names to event output
    cn.names <- eventList[which(es$outcomes=="success"),1]
  }

  ## replace NAs with 0 as it's returns now
  es.w <- na.fill(es.w, 0)

  if(length(cn.names)==1){
    cat("Event date exists only for",cn.names,"\n")
    if (inference == TRUE) {
      warning("No inference strategy for one column","\n")
      inference <- FALSE
    }
  } else if (length(cn.names) == 0) {
    ## skip everything
    to.remap = FALSE
    inference = FALSE
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

  final.result <- list(eventstudy.output = result,
                       outcomes = as.character(es$outcomes))

  attr(final.result, which = "inference") <- inference.strategy
  attr(final.result, which = "event.window") <- event.window
  attr(final.result, which = "remap") <- remapping

  class(final.result) <- "es"
  return(final.result)
}

#########################
## Functions for class es
#########################

print.es <- function(x, ...){
  cat("Event study", colnames(x$eventstudy.output)[2], "response with",
      attr(x, "inference"), "inference for CI:\n")
  print(x$eventstudy.output)
  cat("\n","Event outcome has",length(which(x$outcomes=="success")),
      "successful outcomes out of", length(x$outcomes),"events:","\n")
  print(x$outcomes)
}

summary.es <- function(object, ...){
    print.es(object, ...)
}

plot.es <- function(x, xlab = NULL, ylab = NULL, ...){
  if (NCOL(x$eventstudy.output) < 3) {
      cat("Error: No confidence bands available to plot.\n")
      return(invisible(NULL))
  }
  big <- max(abs(x$eventstudy.output))
  hilo <- c(-big,big)
  width <- (nrow(x$eventstudy.output)-1)/2

  ## assign own labels if they're missing
  if (is.null(ylab)) {
      if (attr(x, "remap") == "cumsum") {
          remapLabel <- "Cum."
      } else if (attr(x, "remap") == "cumprod") {
          remapLabel <- "Cum. product"
      } else if (attr(x, "remap") == "reindex") {
          remapLabel <- "Re-index"
      } else {
          remapLabel <- ""
      }
      ylab <- paste0("(", remapLabel, ")", " change in response series (%)")
  }

  if (is.null(xlab)) {
      xlab <- "Event time"
  }

  plot(-width:width, x$eventstudy.output[,2], type="l", lwd=2, ylim=hilo,
       xlab = xlab, ylab = ylab, ...)

  points(-width:width, x$eventstudy.output[,2])
  lines(-width:width, x$eventstudy.output[,"2.5%"],
        lwd=1, lty=2, ...)
  lines(-width:width, x$eventstudy.output[,"97.5%"],
        lwd=1, lty=2, ...)
  abline(h=0,v=0)
}
