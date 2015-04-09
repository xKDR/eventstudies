library(testthat)
context("userinput")

## 1. Testing remap.cumsum for univariate series i.e. only one firm
## 2. Testing remap.cumprod for univariate series i.e. only one firm
## 3. Testing remap.cumsum for percentage and absolute returns
## 4. Testing remap.cumprod for percentage and absolute returns

test_that("userinput for remap.functions", {
  library(eventstudies)
  ## Data of Stock Prices
  test.data <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
                           35.89, 36.19, 37.1317, 36.7033, 37.7933,
                           37.8533, 285.325, 292.6, 290.025, 286.2,
                           290.075, 295.05, 289.325, 285.625, 293.7,
                           298.5, 289.05, 704.5438, 708.35, 735.8375,
                           710.625, 711.65, 731.025, 727.575, 715.187,
                           724.2, 713.1875, 695.1812),
                         .Dim = c(11L, 3L),
                         .Dimnames = list( NULL, c("ITC", "Reliance",
                           "TCS")), index = structure(c(12418,
                                      12419, 12422, 12423, 12424,
                                      12425, 12426, 12429, 12430,
                                      12431, 12432),
                                      class = "Date"),
                         class = "zoo")
  test.data <- diff(log(test.data))
  ## List of events
  test.eventslist <- data.frame(name=c("ITC","Reliance","TCS",
                                  "ITC","Reliance","Junk"),
                                when=as.Date(c("2004-01-02",
                                  "2004-01-08", "2004-01-14",
                                  "2005-01-15", "2004-01-01",
                                  "2005-01-01")))
  test.eventslist$name <- as.character(test.eventslist$name)

  ## Converting in the window format, as returned by phys2eventtime

  esConvertNormal0 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 2)
  esConvertNormal1 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 1)
  es.test.w0 <- window(esConvertNormal0$z.e, start = -4, end = +4)
  es.test.w1 <- window(esConvertNormal1$z.e, start = -1, end = +1)

  ### Testing remap.cumsum for univariate series i.e. only one firm

  cat("\nTesting for univariate series in remap.cumsum")
  es.test.remap0 <- remap.cumsum(es.test.w0, is.pc = FALSE, base = 0)
  es.test.remap1 <- remap.cumsum(es.test.w1, is.pc = FALSE, base = 0)

  ### Testing remap.cumprod for univariate series i.e. only one firm

  cat("\nTesting for univariate series in remap.cumsum")
  es.test.remap2 <- remap.cumprod(es.test.w0, is.pc = FALSE,
                                  base = 100)
  es.test.remap3 <- remap.cumprod(es.test.w1, is.pc = FALSE,
                                  base = 100)

  ### Testing remap.cumsum for percentage and absolute returns
  
  cat("\nTesting for percentage and absolute returns in remap.cumsum")
  
  es.test.remap4 <- remap.cumsum(es.test.w0, is.pc = TRUE, base = 0)
  
  es.test.remap5 <- remap.cumsum(es.test.w0, is.pc = FALSE, base = 0)

  ### Testing remap.cumprod for percentage and absolute returns
  
  cat("\nTesting for percentage and absolute returns in remap.cumprod")
  
  es.test.remap4 <- remap.cumsum(es.test.w0, is.pc = TRUE, base = 100)
  
  es.test.remap5 <- remap.cumsum(es.test.w0, is.pc = FALSE,
                                 base = 100)
})
