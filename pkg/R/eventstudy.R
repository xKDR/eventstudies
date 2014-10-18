eventstudy <- function(firm.returns,
                       event.list,
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
      stop("model.args cannot be NULL when 'type' is not 'None'.")
  }

  if (is.levels == TRUE) {
    firm.returns <- diff(log(firm.returns)) * 100
  }

  ## handle single series
  if (is.null(ncol(firm.returns))) {
      stop("firm.returns should be a zoo series with at least one column. Use '[' with 'drop = FALSE'.")
  }

  stopifnot(!is.null(remap))

                                        # compute estimation and event period
  ## :DOC: event period starts from event time + 1
  event.period <- as.character((-event.window + 1):event.window) #XXX

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

### marketModel
  if (type == "marketModel") {
    cat("preparing paramters\n")
    prepare.returns(event.list = event.list,
                    event.window = event.window,
                    list(firm.returns = firm.returns, market.returns = model.args$market.returns))

    if (all(outcomes != "success")) {
      ## skip everything
      to.remap = FALSE
      inference = FALSE
    } else {
      outputModel <- lapply(1:NCOL(firm.returns.eventtime$z.e), function(col) {
      cat("col", col, "\n")
      ## :DOC: convert market.returns to eventtime so that we can use
      ## model easily on estimation period and event period
      market.returns.event.list <- data.frame(name = "market.returns",
                                              when = firm.returns.eventtime$events[col])
                                        # these returns also need to be converted to event time
      market.returns <- other.returns[, "market.returns"]
      if (is.null(ncol(market.returns))) {
        attr(market.returns, which = "dim") <- c(length(market.returns) , 1)
        attr(market.returns, which = "dimnames") <- list(NULL, "market.returns")
      }
      market.returns.eventtime <- phys2eventtime(z = market.returns,
                                                 events = market.returns.event.list,
                                                 width = 0) # width doesn't matter for this
      colnames(market.returns.eventtime$z.e) <- colnames(firm.returns.eventtime$z.e)[col]

      model <- marketModel(firm.returns.eventtime$z.e[estimation.period, col],
                           market.returns.eventtime$z.e[estimation.period, ])

      abnormal.returns <- firm.returns.eventtime$z.e[event.period, col] -
          model$coefficients[1] - (model$coefficients[2] * market.returns.eventtime$z.e[event.period, ])

      return(abnormal.returns)
      })

      if (is.null(outputModel)) {
        cat("Error: marketModel() returned NULL\n")
        return(NULL)
      }

      if (length(outputModel) != 1) {
        outputModel <- do.call(cbind, outputModel)
        names(outputModel) <- colnames(firm.returns.eventtime$z.e)
      }
    } ## END else
  } ## END marketModel


### excessReturn
  if (type == "excessReturn") {
    outputModel <- excessReturn(firm.returns, model.args$market.returns)
    if (is.null(outputModel)) {
      cat("Error: excessReturn() returned NULL\n")
      return(NULL)
    }
  }


  if(NCOL(outputModel) == 1) {
    name <- event.list[outcomes == "success", "name"]
    event.number <- rownames(event.list[outcomes == "success", ])
    cat("Event date exists only for", name,"\n")
    attr(outputModel, which = "dim") <- c(1 , 1)
    attr(outputModel, which = "dimnames") <- list(NULL, event.number)
    if (inference == TRUE) {
      warning("No inference strategy for single successful event.","\n")
      inference <- FALSE
    }
  }


### Remapping event frame
  if (to.remap == TRUE) {
    es.w <- switch(remap,
                   cumsum = remap.cumsum(outputModel, is.pc = FALSE, base = 0),
                   cumprod = remap.cumprod(outputModel, is.pc = TRUE,
                     is.returns = TRUE, base = 100),
                   reindex = remap.event.reindex(outputModel)
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
    result <- outputModel
  }
  if(to.remap==TRUE){remapping <- remap} else {remapping <- "none"}

  final.result <- list(result = result,
                       outcomes = as.character(outcomes))

  attr(final.result, which = "inference") <- inference.strategy
  attr(final.result, which = "event.window") <- event.window
  attr(final.result, which = "remap") <- remapping

  class(final.result) <- "es"
  return(final.result)
}

## return values:
## 1. other.returns: data.frame
## 2. firm.returns.eventtime: data.frame
## 3. outcomes: vector
## 4. estimation.period: vector
prepare.returns <- function(event.list, event.window, ...) {
  returns <- unlist(list(...), recursive = FALSE)

  ## :DOC:to pick out the common dates of data. can't work on event
  ## time if the dates of data do not match before converting to
  ## event time.
  returns.zoo <- do.call(merge.zoo, append(returns, values = list(all = TRUE, fill = NA)))

  other.returns.names <- names(returns)[-match("firm.returns", names(returns))]
  other.returns <- returns.zoo[, other.returns.names]
  assign("other.returns", value = other.returns, envir = parent.frame())

  returns.zoo <- returns.zoo[, -match(other.returns.names, colnames(returns.zoo))]
  firm.returns.eventtime <- phys2eventtime(z = returns.zoo, events = event.list, width = event.window)
  firm.returns.eventtime$z.e <- na.fill(firm.returns.eventtime$z.e, 0) # :DOC
  assign("firm.returns.eventtime", value = firm.returns.eventtime, envir = parent.frame())
  assign("outcomes", value = firm.returns.eventtime$outcomes, envir = parent.frame())

  ## :DOC: estimation period goes till event time (inclusive)
  assign("estimation.period",
         value = as.character(index(firm.returns.eventtime$z.e)[1]:(-event.window)),
         envir = parent.frame())
}

#########################
## Functions for class es
#########################

print.es <- function(x, ...){
  cat("Event study", colnames(x$result)[2], "response with",
      attr(x, "inference"), "inference for CI:\n")
  print(x$result)
  cat("\n","Event outcome has",length(which(x$outcomes=="success")),
      "successful outcomes out of", length(x$outcomes),"events:","\n")
  print(x$outcomes)
}

summary.es <- function(object, ...){
    print.es(object, ...)
}

plot.es <- function(x, xlab = NULL, ylab = NULL, ...){
  if (NCOL(x$result) < 3) {
      cat("Error: No confidence bands available to plot.\n")
      return(invisible(NULL))
  }
  big <- max(abs(x$result))
  hilo <- c(-big,big)
  width <- (nrow(x$result)-1)/2

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

  plot(-width:width, x$result[,2], type="l", lwd=2, ylim=hilo,
       xlab = xlab, ylab = ylab, ...)

  points(-width:width, x$result[,2])
  lines(-width:width, x$result[,"2.5%"],
        lwd=1, lty=2, ...)
  lines(-width:width, x$result[,"97.5%"],
        lwd=1, lty=2, ...)
  abline(h=0,v=0)
}
