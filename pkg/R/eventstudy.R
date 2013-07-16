eventstudy <- function(inputData = NULL,
                       eventList,
                       width = 10,
                       type = "marketResiduals",
                       to.remap = TRUE,
                       remap = "cumsum",
                       to.plot = TRUE,
                       ...) {
                                        # type = "marketResiduals", "excessReturn", "AMM", "None"
  if (type == "None" && !is.null(inputData)) {
    outputModel <- inputData
  } else {
    stop("inputData or \"None\" type missing")
  }

### Run models
  ## AMM
  if (type == "AMM") {
    outputModel <- AMM(...)
  }

  ## marketResiduals
  if (type == "marketResiduals") {
    outputModel <- marketResiduals(...)
  }

  ## excessReturn
  if (type == "excessReturn") {
    outputModel <- excessReturn(...)
  }

### Convert to event frame
  es <- phys2eventtime(z=outputModel, events=eventList, width=width)
  es.w <- window(es$z.e, start = -width, end = width)

### Remapping event frame
  if (to.remap == TRUE) {
    es.w <- switch(remap,
                   cumsum = remap.cumsum(es.w, is.pc = FALSE, base = 0),
                   cumprod = remap.cumprod(es.w, is.pc = TRUE, is.returns = TRUE, base = 100),
                   reindex = remap.event.reindex(es.w)
                   )
  }

### Bootstrap
  result <- inference.Ecar(z.e = es.w, to.plot = to.plot)
