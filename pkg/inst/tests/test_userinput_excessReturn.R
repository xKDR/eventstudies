library(testthat)
context("userinput")

## 1. Test for number of rows in dataframe of firm returns and
##    market returns should be equal.

test_that("userinput for excessReturn", {
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
  

  ## Data for market prices and returns
  test.market <- structure(c(285.546, 265.566, 290.025, 288.2,
                             295.677, 298.990, 279.32, 286.62,
                             296.7, 288.5, 284.05),
                           .Dim = c(11L, 1L),
                           .Dimnames = list( NULL, c("MarketIndex")),
                           index = structure(c(12418,
                             12419, 12422, 12423, 12424,
                             12425, 12426, 12429, 12430,
                             12431, 12432),
                             class = "Date"),
                           class = "zoo")
  test.market <- diff(log(test.market))
  
  ## List of events
  test.eventslist <- data.frame(name=c("ITC","Reliance","TCS",
                                  "ITC","Reliance","Junk"),
                                when=as.Date(c("2004-01-02",
                                  "2004-01-08", "2004-01-14",
                                  "2005-01-15", "2004-01-01",
                                  "2005-01-01")))
  test.eventslist$name <- as.character(test.eventslist$name)
  
### Testing the function for number of rows in dataframe of
### firm returns and market returns                       

  cat("\n Testing for no. of rows in firm returns and market returns")
  test.market1 <- test.market[-1,] 
  er.testResult1 <- excessReturn(firm.returns = test.firm,
                                 market.returns = test.market[,1])

  
  er.testResult2 <- excessReturn(firm.returns = test.firm,
                                 market.returns = test.market1[,1])

})

  
      
