library(testthat)
context("functionality")

## 1. Test for output returned from each of the three strategies
## 1a. Class of the object returned
## 1b. Number of data points
## 1c. Check for mean estimate and confidence intervals
test_that("functionality for inference functions", {
  library(eventstudies)
  ## Data of Stock Prices
  test.data0 <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
                           35.89, 36.19, 37.1317, 36.7033, 37.7933,
                           37.8533, 285.325, 292.6, 290.025, 286.2,
                           290.075, 295.05, 289.325, 285.625, 293.7,
                           298.5, 289.05, 704.5438, 708.35, 735.835,
                           710.625, 711.65, 731.012, 727.575,
                           715.0187, 724.2, 713.1875, 695.1812),
                         .Dim = c(11L, 3L),
                         .Dimnames = list( NULL, c("ITC",
                           "Reliance", "TCS")),
                         index = structure(c(12418,
                           12419, 12422, 12423, 12424,
                           12425, 12426, 12429, 12430,
                           12431, 12432),
                           class = "Date"),
                         class = "zoo")
  test.data0 <- diff(log(test.data0))

  ## List of events
  test.eventslist0 <- data.frame(name = c("ITC","Reliance","TCS",
                                   "ITC","Reliance","Junk"),
                                 when = as.Date(c("2004-01-02",
                                   "2004-01-08", "2004-01-14",
                                   "2005-01-15", "2004-01-01",
                                   "2005-01-01")))
  test.eventslist0$name <- as.character(test.eventslist0$name)

  esConvertNormal0 <- phys2eventtime(z = test.data0,
                                     events = test.eventslist0,
                                     width = 1)
  es.test.w0 <- window(esConvertNormal0$z.e,
                       start = -1,
                       end = +1)
  test.eventtime0 <- remap.cumsum(es.test.w0, is.pc = FALSE,
                                  base = 0)
                                        # Bootstrap strategy
  test.boot <- inference.bootstrap(es.w = test.eventtime0,
                                   to.plot = FALSE)
                                        # Wilcox strategy
  test.wilcox <- inference.wilcox(es.w = test.eventtime0,
                                  to.plot = FALSE)
                                        # Classic strategy
  test.classic <- inference.classic(es.w = test.eventtime0,
                                    to.plot = FALSE)
  testing.inference <- function(strategy){

      cat("\nTesting for class of the object returned")

      expect_that(length(which(colnames(strategy) %in% c("2.5%",
                                                         "Mean",
                                                         "Median",
                                                         "97.5%"))),
                  equals(3))

      cat("\nTestimg for number of data points returned in the object")
      expect_that(nrow(strategy), equals(nrow(test.eventtime0)))
      
      cat("\nTesting for mean estimates and confidence intervals")
      expect_that(class(strategy), equals("matrix"))
  }

  testing.inference(strategy = test.boot)
  testing.inference(strategy = test.wilcox)
  testing.inference(strategy = test.classic)
  
})
