library(testthat)
context("userinput")

## 1. Test for normal values of width
## 2. Test for univariate zoo object: matrix and not a vector


test_that("userinput for phys2eventtime", {
  library(eventstudies)
## Data of Stock Prices 
test.data <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
                         35.89, 36.19, 37.1317, 36.7033, 37.7933,
                         37.8533, 285.325, 292.6, 290.025, 286.2,
                         290.075, 295.05, 289.325, 285.625, 293.7,
                         298.5, 289.05, 704.5438, 708.35, 735.8375,
                         710.625, 711.65, 731.0125, 727.575, 715.0187,
                         724.2, 713.1875, 695.1812),
                       .Dim = c(11L, 3L),
                       .Dimnames = list( NULL, c("ITC", "Reliance",
                         "TCS")), index = structure(c(12418,
                                        12419, 12422, 12423, 12424,
                                        12425, 12426, 12429, 12430,
                                        12431, 12432),
                                        class = "Date"),
                       class = "zoo")
## List of events
test.eventslist <- data.frame(name=c("ITC","Reliance","TCS",
                                "ITC","Reliance","Junk"),
                              when=as.Date(c("2004-01-02",
                                "2004-01-08", "2004-01-14",
                                "2005-01-15", "2004-01-01",
                                "2005-01-01")))
test.eventslist$name <- as.character(test.eventslist$name)


### Testing function for normal values of width

  cat("\nTesting for normal input values")
  esConvertNormal0 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 5)
  cat("\nTesting for normal input values")
  esConvertNormal1 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 10)

### Testing for univariate zoo object: matrix and not a vector

  test.data2 <- test.data[,1, drop=FALSE]
  test.data3 <- test.data[,1]
  cat("\nTesting for univariate zoo object")
  esConvertNormal2 <- phys2eventtime(z = test.data2,
                                     events = test.eventslist,
                                     width = 2)
  
  esConvertNormal3 <- phys2eventtime(z = test.data3,
                                     events = test.eventslist,
                                     width = 2)
  
})

