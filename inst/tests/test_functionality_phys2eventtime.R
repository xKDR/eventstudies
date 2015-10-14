library(testthat)
context("functionality")

# 1. Test for class of arguments 
# 2. Test for missing data in eventlist
# a. NAs in firm names
# b. NAs in event dates
# 3. Testing the functionality of "phys2eventtime" for components of
#    the list returned from the function:
# a. Elements in outcomes
# b. Elements in z.e
# 4. Testing that firms should not have NA for defined width of
#    phys2eventtime
# a. Test for wrongspan functionality
# b. Test for wdatamissing functionality
# 5. Test for only one observation in events list.
test_that("functionality for phys2eventtime", {
  library(eventstudies)

### Data for testing ###

  test.data <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
                           35.89, 36.19, 37.1317, 36.7033, 37.7933,
                           37.8533, 285.325, 292.6, 290.025, 286.2,
                           290.075, 295.05, 289.325, 285.625, 293.7,
                           298.5, 289.05, 704.5438, 708.35, 735.8375,
                           710.625, 711.65, 731.0125, 727.575, 715.01,
                           724.2, 713.1875, 695.1812),
                         .Dim = c(11L, 3L),
                         .Dimnames = list( NULL, c("ITC", "Reliance",
                           "TCS")), index = structure(c(12418,
                                      12419, 12422, 12423, 12424,
                                      12425, 12426, 12429, 12430,
                                      12431, 12432),
                                      class = "Date"),
                         class = "zoo")
  
### List of events
  
  test.eventslist <- data.frame(name = c("ITC","Reliance","TCS",
                                  "ITC","Reliance","Junk"),
                                when = as.Date(c("2004-01-02",
                                  "2004-01-08", "2004-01-14",
                                  "2005-01-15", "2004-01-01",
                                  "2005-01-01")))
  test.eventslist$name <- as.character(test.eventslist$name)
                 
### Test for class of arguments

  cat("\nTesting for class of arguments input")

  test.data1 <- structure(c(33.16, 34.0967, 35.3683, 34.46, 34.17,
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
  test.eventslist1 <- as.matrix(test.eventslist)
  expect_error(esConvertNormal0 <- phys2eventtime(z = test.data1,
                                     events = test.eventslist,
                                     width = 2))

  expect_error(esConvertNormal0 <- phys2eventtime(z = test.data,
                                     events = test.eventslist1,
                                     width = 2))

### Testing for missing data in eventslist

  cat("\nTesting for missing dates in eventslist")
  test.eventslist2 <- data.frame(name = c("ITC","Reliance","TCS",
                                   "ITC","Reliance","Junk"),
                                 when = as.Date(c("2004-01-02",
                                   "2004-01-08", "2004-01-14",
                                   NA, "2004-01-01",
                                   "2005-01-01")))
  test.eventslist2$name <- as.character(test.eventslist2$name)
  expect_error(esConvertNormal1 <- phys2eventtime(z = test.data,
                                     events = test.eventslist2,
                                     width = 2))

  cat("\nTesting for missing firm names in eventlist")
  test.eventslist2 <- data.frame(name = c("ITC",NA,"TCS",
                                   "ITC","Reliance","Junk"),
                                 when = as.Date(c("2004-01-02",
                                   "2004-01-08", "2004-01-14",
                                   "2005-01-15", "2004-01-01",
                                   "2005-01-01")))
  test.eventslist2$name <- as.character(test.eventslist2$name)
  expect_error(esConvertNormal2 <- phys2eventtime(z = test.data,
                                     events = test.eventslist2,
                                     width = 2))

### Testing the function for outcomes

  cat("\nTesting for list component: outcomes")
  esConvertNormal3 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 10)
  expect_that(length(esConvertNormal3$outcomes),
              equals(nrow(test.eventslist)))

  cat("\nTesting for list component: z.e")
  analyseDate <- subset(test.eventslist,
                        test.eventslist$when >= index(test.data[1,]))
  maxDate <- max(as.Date(analyseDate$when))
  minDate <- min(as.Date(analyseDate$when))
  l1 <- length(which(index(test.data) <= maxDate))
  l2 <- length(which(index(test.data) > minDate))
  elementsInz.e <- l1 + l2
  if(is.null(esConvertNormal3$z.e))
    {expect_that(nrow(esConvertNormal3$z.e), equals(NULL))
   }else{
  expect_that(nrow(esConvertNormal3$z.e), equals(elementsInz.e))}
  
### Testing that firms should not have NA for defined width

  cat("\nTesting for no NA values in defined width")
  esConvertNormal4 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 2)
  expect_that(esConvertNormal4$z.e[8:12], not(equals(NA)))

  cat("\nTesting for wrongspan")
  esConvertNormal8 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 3)
  expect_that(as.character(esConvertNormal8$outcomes[1]), equals("wrongspan"))

  cat("\nTesting for wdatamissing")
  test.data.NA <- test.data
  test.data.NA[1, 1] <- NA
  esConvertNormal8 <- phys2eventtime(z = test.data.NA,
                                     events = test.eventslist,
                                     width = 2)
  expect_that(as.character(esConvertNormal8$outcomes[1]), equals("wdatamissing"))

### Testing for output window
  cat("\nTesting for output window for any successful event")
  esConvertNormal9 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 2)
  if (any(esConvertNormal9$outcomes == "success")) {
      expect_that(index(esConvertNormal9$z.e[as.character(-1:2), ]),
                  equals(-1:2))
  }
  

### Testing for only one firm in eventslist

  test.eventslist3 <- test.eventslist[1,]            
  cat("\nTesting for only one firm data in events list")
  expect_error(esConvertNormal7 <- phys2eventtime(z = test.data1,
                                     events = test.eventslist3,
                                     width = 2))

})
