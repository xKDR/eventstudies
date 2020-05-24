library(testthat)
context("functionality")

## 1. Test for class of arguments

test_that("functionality for constantMeanReturn", {
  library(eventstudies)
  ## Data of stock prices and returns
  test.firm <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
                           35.89, 36.19, 37.1317, 36.7033, 37.7933,
                           37.8533, 285.325, 292.6, 290.025, 286.2,
                           290.075, 295.05, 289.325, 285.625, 293.7,
                           298.5, 289.05, 704.5438, 708.35, 735.835,
                           710.6, 711.65, 731.012, 727.57, 715.0187,
                           724.2, 713.1875, 695.1812),
                         .Dim = c(11L, 3L),
                         .Dimnames = list( NULL, c("ITC",
                           "Reliance", "TCS")),
                         index = structure(c(12418,
                           12419, 12422, 12423, 12424,
                           12425, 12426, 12429, 12430,
                           12431, 12432),
                           class = "Date"),
                         class = "zoo")
  test.firm <- diff(log(test.firm))
  
  test.firm1 <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
                            35.89, 36.19, 37.1317, 36.7033, 37.7933,
                            37.8533, 285.325, 292.6, 290.025, 286.2,
                            290.075, 295.05, 289.325, 285.625, 293.7,
                            298.5, 289.05, 704.5438, 708.35, 735.8375,
                            710.625, 711.65, 731.013, 727.575, 715.01,
                            724.2, 713.1875, 695.1812),
                          .Dim = c(11L, 3L),
                          .Dimnames = list( NULL, c("ITC", "Reliance",
                            "TCS")), index = structure(c(12418,
                                       12419, 12422, 12423, 12424,
                                       12425, 12426, 12429, 12430,
                                       12431, 12432),
                                       class = "Date"),
                          class = "matrix")
  test.firm1 <- diff(log(test.firm1))

  ## List of events
  test.eventslist <- data.frame(name = c("ITC","Reliance","TCS",
                                    "ITC","Reliance"),
                                when = as.Date(c("2004-01-02",
                                    "2004-01-08", "2004-01-09",
                                    "2005-01-15", "2004-01-08")))
  test.eventslist$name <- as.character(test.eventslist$name)
  

  ## Testing the class of arguments
  cat("\nTesting for class of arguments input")
  es.result <- phys2eventtime(test.firm, test.eventslist, 2)
  estimation.period <- attributes(es.result$z.e)$index[1]:-2
  cmr <- constantMeanReturn(es.result$z.e[which(attributes(es.result$z.e)$index %in% estimation.period), ], residual = FALSE)

})
