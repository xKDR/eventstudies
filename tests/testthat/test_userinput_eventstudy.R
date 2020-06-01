library(testthat)
context("wrapper")

## 1. Testing for normal input values
## 2. Testing for wrong class of returns object
## 3. Testing for event window less than 1
## 4. Testing for value of is.levels with input in returns format
## 5. Testing for type != None and no model arguments specified

test_that("tests for wrapper function", {
  library(eventstudies)
  ## Data of Stock Prices
  test.data <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
                           35.89, 36.19, 37.1317, 36.7033, 37.7933,
                           37.8533, 285.325, 292.6, 290.025, 286.2,
                           290.075, 295.05, 289.325, 285.625, 293.7,
                           298.5, 289.05, 704.5438, 708.35, 735.8375,
                           710.625, 711.65, 731.0125, 727.575, 715.07,
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
  
  test.data1 <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
                            35.89, 36.19, 37.1317, 36.7033, 37.7933,
                            37.8533, 285.325, 292.6, 290.025, 286.2,
                            290.075, 295.05, 289.325, 285.625, 293.7,
                            298.5, 289.05, 704.5438, 708.35, 735.8375,
                            710.625, 711.65, 731.0125, 727.575, 715.7,
                            724.2, 713.1875, 695.1812),
                          .Dim = c(11L, 3L),
                          .Dimnames = list( NULL, c("ITC", "Reliance",
                            "TCS")), index = structure(c(12418,
                                       12419, 12422, 12423, 12424,
                                       12425, 12426, 12429, 12430,
                                       12431, 12432),
                                       class = "Date"),
                          class = "matrix")
  test.data1 <- diff(log(test.data1))


  ## List of events
  test.eventslist <- data.frame(name = c("ITC","Reliance","TCS",
                                  "ITC","Reliance","Junk"),
                                when = as.Date(c("2004-01-02",
                                  "2004-01-08", "2004-01-14",
                                  "2005-01-15", "2004-01-01",
                                  "2005-01-01")))
  test.eventslist$name <- as.character(test.eventslist$name)

  ## Market returns
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
  
#### Testing for argument inputs ####

  ## for normal values
  message("Testing for normal input values")
  es.test.wrapper1 <- eventstudy(firm.returns = test.data,
                                 event.list = test.eventslist,
                                 event.window = 1,
                                 is.levels =  FALSE,
                                 type = "None",
                                 to.remap = TRUE,
                                 remap = "cumsum",
                                 inference = TRUE,
                                 inference.strategy = "bootstrap",
                                 model.args = NULL)  

  ## for wrong class of returns object
  message("Testing for wrong class of returns object")
  expect_error(eventstudy(firm.returns = test.data1,
                          event.list = test.eventslist,
                          event.window = 1,
                          is.levels =  FALSE,
                          type = "None",
                          to.remap = TRUE,
                          remap = "cumsum",
                          inference = TRUE,
                          inference.strategy = "bootstrap",
                          model.args = NULL))

  ## for event window less than 1
  message("Testing for event window less than 1")
  expect_error(eventstudy(firm.returns = test.data,
                          event.list = test.eventslist,
                          event.window = -1,
                          is.levels =  FALSE,
                          type = "None",
                          to.remap = TRUE,
                          remap = "cumsum",
                          inference = TRUE,
                          inference.strategy = "bootstrap",
                          model.args = NULL))

  ## for is.levels = TRUE when data is already in returns format
  message("Testing for value of is.levels with input in returns format")
  expect_error(eventstudy(firm.returns = test.data,
                          event.list = test.eventslist,
                          event.window = -1,
                          is.levels =  TRUE,
                          type = "None",
                          to.remap = TRUE,
                          remap = "cumsum",
                          inference = TRUE,
                          inference.strategy = "bootstrap",
                          model.args = NULL))

  ## for type != None and no model arguments specified
  message("Testing for type != None and no model arguments specified")
  expect_error(eventstudy(firm.returns = test.data,
                          event.list = test.eventslist,
                          event.window = 1,
                          is.levels =  TRUE,
                          type = "excessReturn",
                          to.remap = TRUE,
                          remap = "cumsum",
                          inference = TRUE,
                          inference.strategy = "bootstrap",
                          model.args = NULL))

})
