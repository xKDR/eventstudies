eventstudy <- function(firm.returns,
                       event.list,
                       event.window = 10,
                       is.levels =  FALSE,
                       type = "marketModel",
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
  event.period <- as.character((-event.window + 1):event.window)

### Run models
  ## AMM
  if (type == "lmAMM") {

    cat("preparing paramters\n")
    if (length(dim(model.args$market.returns)) == 2) {
        colnames(model.args$market.returns) <- "market.returns" # needed to fix market returns colname
    }
    returns.zoo <- prepare.returns(event.list = event.list,
                                   event.window = event.window,
                                   list(firm.returns = firm.returns,
                                        market.returns = model.args$market.returns,
                                        others = model.args$others))

    outcomes <- do.call(c, sapply(returns.zoo, '[', "outcomes"))
    names(outcomes) <- gsub(".outcomes", "", names(outcomes))

    if (all(unique(outcomes) != "success")) {
      cat("Error: no successful events\n")
      to.remap = FALSE
      inference = FALSE
      outputModel <- NULL
    } else {
      returns.zoo <- returns.zoo[which(outcomes == "success")]
      outputModel <- lapply(returns.zoo, function(firm) {
        if (is.null(firm$z.e)) {
          return(NULL)
        }
        estimation.period <- attributes(firm)[["estimation.period"]]

        ## Estimating AMM regressors
        args.makeX <- list()
        if (!is.null(model.args$nlag.makeX)) {
            args.makeX$nlags <- model.args$nlag.makeX
        }
        names.args.makeX <- names(model.args)[names(model.args) %in% formalArgs(makeX)]
        names.args.makeX <- names.args.makeX[-match("market.returns", names.args.makeX)]
        names.args.makeX <- names.args.makeX[-match("others", names.args.makeX)]
        args.makeX <- append(args.makeX, model.args[names.args.makeX])

        names.nonfirmreturns <- colnames(firm$z.e)[!colnames(firm$z.e) %in% c("firm.returns", "market.returns")]
        args.makeX$market.returns <- na.locf(firm$z.e[estimation.period, "market.returns"], na.rm = FALSE) #XXX REMOVE
        args.makeX$others <- na.locf(firm$z.e[estimation.period, names.nonfirmreturns], na.rm = FALSE)
        regressors <- do.call(makeX, args.makeX)

        args.lmAMM <- list()
        if (!is.null(model.args$nlag.lmAMM)) {
            args.lmAMM$nlags <- model.args$nlag.lmAMM
        }
        args.lmAMM <- append(args.lmAMM, model.args[names(model.args) %in% formalArgs(lmAMM)])
        args.lmAMM$firm.returns <- na.locf(firm$z.e[estimation.period, "firm.returns"], na.rm = FALSE) #XXX REMOVE na.locf(), its just done to get a regular residuals series.
        args.lmAMM$X <- regressors

        model <- do.call(lmAMM, args.lmAMM)
        if (is.null(model)) {
            return(NULL)
        }

        abnormal.returns <- firm$z.e[event.period, "firm.returns"] - model$coefficients["(Intercept)"] -
            (model$exposures["market.returns"] * firm$z.e[event.period, "market.returns"])

        for (i in 2:length(model$exposures)) { # 2: not market returns
            abnormal.returns <- abnormal.returns - (model$exposures[i] * firm$z.e[event.period, names.nonfirmreturns[i - 1]])
        }

        attr(abnormal.returns, "residuals") <- model$residuals
        return(abnormal.returns)
      })

      ## remove the NULL values
      null.values <- sapply(outputModel, is.null)
      if (length(which(null.values)) > 0) {
        outputModel <- outputModel[names(which(!null.values))]
        outcomes[names(which(null.values))] <- "edatamissing" #:DOC: edatamissing: estimation data missing
      }

      if (length(outputModel) == 0) {
          warning("lmAMM() returned NULL\n")
          outputModel <- NULL
      } else {
        outputResiduals <- lapply(outputModel, function(x) attributes(x)[["residuals"]])
        outputModel <- do.call(merge.zoo, outputModel)
      }
    }
  } ## end AMM

### marketModel
  if (type == "marketModel") {
    cat("preparing paramters\n")
    if (length(dim(model.args$market.returns)) == 2) {
        colnames(model.args$market.returns) <- "market.returns" # needed to fix market returns colname
    }
    returns.zoo <- prepare.returns(event.list = event.list,
                                   event.window = event.window,
                                   list(firm.returns = firm.returns,
                                        market.returns = model.args$market.returns))

    outcomes <- do.call(c, sapply(returns.zoo, '[', "outcomes"))
    names(outcomes) <- gsub(".outcomes", "", names(outcomes))

    if (all(unique(outcomes) != "success")) {
      cat("Error: no successful events\n")
      to.remap = FALSE
      inference = FALSE
      outputModel <- NULL
    } else {
      returns.zoo <- returns.zoo[which(outcomes == "success")]
      outputModel <- lapply(returns.zoo, function(firm) {
        if (is.null(firm$z.e)) {
          return(NULL)
        }
        estimation.period <- attributes(firm)[["estimation.period"]]
        model <- marketModel(na.locf(firm$z.e[estimation.period, "firm.returns"], na.rm = FALSE), #XXX: remove na.locf
                             na.locf(firm$z.e[estimation.period, "market.returns"], na.rm = FALSE), #XXX: remove na.locf
                             residuals = FALSE)

          abnormal.returns <- firm$z.e[event.period, "firm.returns"] - model$coefficients["(Intercept)"] -
          (model$coefficients["market.returns"] * firm$z.e[event.period, "market.returns"])

        attr(abnormal.returns, "residuals") <- model$residuals
        return(abnormal.returns)
      })

      null.values <- sapply(outputModel, is.null)
      if (length(which(null.values)) > 0) {
        outputModel <- outputModel[names(which(!null.values))]
        outcomes[names(which(null.values))] <- "edatamissing"
      }

      if (length(outputModel) == 0) {
        warning("marketModel() returned NULL")
        outputModel <- NULL
      } else {
        outputResiduals <- lapply(outputModel, function(x) attributes(x)[["residuals"]])
        outputModel <- do.call(merge.zoo, outputModel)
      }
    }

  } ## END marketModel


### excessReturn
  if (type == "excessReturn") {
    cat("preparing paramters\n")
    if (length(dim(model.args$market.returns)) == 2) {
        colnames(model.args$market.returns) <- "market.returns" # needed to fix market returns colname
    }
    returns.zoo <- prepare.returns(event.list = event.list,
                                   event.window = event.window,
                                   list(firm.returns = firm.returns,
                                        market.returns = model.args$market.returns))

    outcomes <- do.call(c, sapply(returns.zoo, '[', "outcomes"))
    names(outcomes) <- gsub(".outcomes", "", names(outcomes))

    if (all(unique(outcomes) != "success")) {
      message("No successful events")
      to.remap = FALSE
      inference = FALSE
      outputModel <- NULL
    } else {
      returns.zoo <- returns.zoo[which(outcomes == "success")]
      outputModel <- lapply(returns.zoo, function(firm) {
        if (is.null(firm$z.e)) {
          return(NULL)
        }
        estimation.period <- attributes(firm)[["estimation.period"]]
        model <- excessReturn(na.locf(firm$z.e[event.period, "firm.returns"]), #XXX: remove na.locf
                              na.locf(firm$z.e[event.period, "market.returns"])) #XXX: remove na.locf

        abnormal.returns <- model
        return(abnormal.returns)
      })

      null.values <- sapply(outputModel, is.null)
      if (length(which(null.values)) > 0) {
        outputModel <- outputModel[names(which(!null.values))]
        outcomes[names(which(null.values))] <- "edatamissing"
      }

      if (length(outputModel) == 0) {
        warning("excessReturn() returned NULL\n")
        outputModel <- NULL
      } else {
        outputModel <- do.call(merge.zoo, outputModel[!sapply(outputModel, is.null)])
      }
    }
  } ## end excessReturn


### None
  if (type == "None") {
      returns.zoo <- prepare.returns(event.list = event.list,
                                     event.window = event.window,
                                     list(firm.returns = firm.returns))

      outcomes <- returns.zoo$outcomes  # its only a single list in this case
      if (all(unique(outcomes) != "success")) {
          cat("Error: no successful events\n")
          to.remap = FALSE
          inference = FALSE
          outputModel <- NULL
      } else {
        outputModel <-  returns.zoo$z.e[event.period]
      }
  } ## end None


  if (is.null(outputModel)) {           #:DOC
    final.result <- list(result = NULL,
                         outcomes = as.character(outcomes))
    class(final.result) <- "es"
    return(final.result)
  } else if (NCOL(outputModel) == 1) {
    event.number <- which(outcomes == "success")
    message("Only one successful event: #", event.number)
    attr(outputModel, which = "dim") <- c(length(outputModel) , 1)
    attr(outputModel, which = "dimnames") <- list(NULL, event.number)
    if (inference == TRUE) {
      warning("No inference strategy for single successful event.","\n")
      inference <- FALSE
    }
  }


### Remapping event frame
  if (to.remap == TRUE) {
    outputModel <- switch(remap,
                   cumsum = remap.cumsum(outputModel, is.pc = FALSE, base = 0),
                   cumprod = remap.cumprod(outputModel, is.pc = TRUE,
                     is.returns = TRUE, base = 100),
                   reindex = remap.event.reindex(outputModel)
                   )
    remapping <- remap
  } else {
    remapping <- "none"
  }
  
### Inference: confidence intervals
  if (inference == TRUE) {
    ## Bootstrap
    if(inference.strategy == "bootstrap"){
      outputModel <- inference.bootstrap(es.w = outputModel, to.plot = FALSE)
    }
    ## Wilcoxon
    if(inference.strategy == "wilcoxon"){
      outputModel <- inference.wilcox(es.w = outputModel, to.plot = FALSE)
    }
  }

  final.result <- list(result = outputModel,
                       outcomes = as.character(outcomes))

  if (exists("outputResiduals")) {      # :DOC
    attr(final.result, which = "model.residuals") <- outputResiduals
  }
  attr(final.result, which = "event.window") <- event.window
  attr(final.result, which = "inference") <- inference
  if (inference == TRUE) {
    attr(final.result, which = "inference.strategy") <- inference.strategy
  }
  attr(final.result, which = "remap") <- remapping

  class(final.result) <- "es"
  return(final.result)
}

## return values:
## 2. firm.returns.eventtime: data.frame
## 3. outcomes: vector
## 4. estimation.period: vector
prepare.returns <- function(event.list, event.window, ...) {
  returns <- unlist(list(...), recursive = FALSE)
  other.returns.names <- names(returns)[-match("firm.returns", names(returns))]

  if (length(other.returns.names) != 0) { # check for type = "None"
  returns.zoo <- lapply(1:nrow(event.list), function(i) {
    cat("i:", i, "\n")
    firm.name <- event.list[i, "name"]
      ## :DOC:to pick out the common dates of data. can't work on
      ## event time if the dates of data do not match before
      ## converting to event time.
                                        # all = FALSE: pick up dates
                                        # for which data is available
                                        # for all types of returns
      firm.merged <- do.call("merge.zoo",
                             c(list(firm.returns = returns$firm.returns[, firm.name]),
                               returns[other.returns.names],
                               all = FALSE, fill = NA))
      ## other.returns.names needs re-assignment here, since "returns"
      ## may have a data.frame as one of the elements, as in case of
      ## lmAMM.
      other.returns.names <- colnames(firm.merged)[-match("firm.returns", colnames(firm.merged))]

      firm.returns.eventtime <- phys2eventtime(z = firm.merged,
                                       events = rbind(
                                           data.frame(name = "firm.returns",
                                                      when = event.list[i, "when"],
                                                      stringsAsFactors = FALSE),
                                           data.frame(name = other.returns.names,
                                                      when = event.list[i, "when"],
                                                      stringsAsFactors = FALSE)),
                                               width = event.window)

    if (any(firm.returns.eventtime$outcomes == "unitmissing")) {
        ## :DOC: there could be NAs in firm and other returns in the merged object
        return(list(z.e = NULL, outcomes = "unitmissing")) # phys2eventtime output object
    }

    if (any(firm.returns.eventtime$outcomes == "wdatamissing")) {
        return(list(z.e = NULL, outcomes = "wdatamissing")) # phys2eventtime output object
    }

    if (any(firm.returns.eventtime$outcomes == "wrongspan")) {
        ## :DOC: there could be NAs in firm and other returns in the merged object
        return(list(z.e = NULL, outcomes = "wrongspan")) # phys2eventtime output object
    }

    firm.returns.eventtime$outcomes <- "success" # keep one value

    colnames(firm.returns.eventtime$z.e) <- c("firm.returns", other.returns.names)
    ## :DOC: estimation period goes till event time (inclusive)
    attr(firm.returns.eventtime, which = "estimation.period") <-
        as.character(index(firm.returns.eventtime$z.e)[1]:(-event.window))

    return(firm.returns.eventtime)
  })
  names(returns.zoo) <- 1:nrow(event.list)

  } else {
    returns.zoo <- phys2eventtime(z = returns$firm.returns,
                                  events = event.list,
                                  width = event.window)
  }

  return(returns.zoo)
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

## XXX: needs fixing for non-inference objects
plot.es <- function(x, xlab = NULL, ylab = NULL, ...){
  if (!attributes(x)$inference) {
    if (NCOL(x$result) < 3) {
        cat("Error: No confidence bands available to plot.\n")
        return(invisible(NULL))
    }
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
