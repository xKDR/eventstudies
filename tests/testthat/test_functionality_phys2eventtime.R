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
# 5. Test for only one observation in events list.
# 6. Test for intraday functionality
# a. Testing for normal functionality
# b. Testing for announcements post trading day

test_that("functionality for phys2eventtime", {
  library(eventstudies)

### Data for testing ###
  ## Daily data
                                        # Zoo object
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
                                        # List of events
  test.eventslist <- data.frame(name = c("ITC","Reliance","TCS",
                                    "ITC","Reliance","Junk"),
                                when = as.Date(c("2004-01-02",
                                    "2004-01-08", "2004-01-14",
                                    "2005-01-15", "2004-01-01",
                                    "2005-01-01")))
  test.eventslist$name <- as.character(test.eventslist$name)

  ## Intra-day data
                                        # Zoo object
   test.data.intraday <- structure(c(0.647207, 0.575475, 0.950237,
                                     0.333410, 0.972791, 0.859014,
                                     0.462836, 0.101042, 0.420174,
                                     0.108433, 0.480622, 0.768775,
                                     0.936763, 0.509670, 0.598303,
                                     0.325926, 0.768188, 0.210077,
                                     0.028643, 0.465721, 0.426375,
                                     0.612842, 0.673736, 0.297408,
                                     0.828808, 0.091200, 0.915891,
                                     0.5374675),
                                   .Dim = c(7L, 4L),
                                   index = structure(c(1262316600,
                                       1262320200, 1262323800,
                                       1262327400,
                                       1262331000, 1262334600,
                                       1262338200),
                                       class = c("POSIXct", "POSIXt"),
                                       tzone = ""),
                                   class = "zoo",
                                   .Dimnames = list(NULL,
                                       c("ITC", "Reliance",
                                         "TCS", "Infosys")))
                                        # List of events
  test.eventslist.intraday <- structure(list(name = c("ITC",
                                                 "Reliance", "TCS",
                                                 "ITC", "Reliance",
                                                 "Junk"),
                                      when = structure(c(1262331000,
                                          1262327400, 1262327400,
                                          1262316600, 1262334600,
                                          1262320200),
                                          class = c("POSIXct",
                                              "POSIXt"), tzone = "")),
                                        .Names = c("name", "when"),
                                        row.names = c(NA, -6L),
                                        class = "data.frame")
    
### Test for class of arguments

  message("Testing for class of arguments input")

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

  message("Testing for missing dates in eventslist")
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

  message("Testing for missing firm names in eventlist")
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

  message("Testing for list component: outcomes")
  esConvertNormal3 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 10)
  expect_that(length(esConvertNormal3$outcomes),
              equals(nrow(test.eventslist)))

  message("Testing for list component: z.e")
  esConvertNormal9 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 5)
  
  analyseDate <- subset(test.eventslist,
                        test.eventslist$when >= (index(test.data[1,]) + 4))
  maxDate <- max(as.Date(analyseDate$when))
  minDate <- min(as.Date(analyseDate$when))
  l1 <- length(which(index(test.data) <= maxDate))
  l2 <- length(which(index(test.data) > minDate))
  elementsInz.e <- l1 + l2
  if(is.null(esConvertNormal3$z.e))
    {expect_that(nrow(esConvertNormal3$z.e), equals(NULL))
   }else{
  expect_that(nrow(esConvertNormal9$z.e), equals(elementsInz.e))}
  
### Testing that firms should not have NA for defined width

  message("Testing for no NA values in defined width")
  esConvertNormal4 <- phys2eventtime(z = test.data,
                                     events = test.eventslist,
                                     width = 2)
  x <- esConvertNormal4$z.e[8:12]
  expect_false(all(is.na(x)))


### Testing for only one firm in eventslist

  test.eventslist3 <- test.eventslist[1,]            
  message("Testing for only one firm data in events list")
  expect_error(esConvertNormal7 <- phys2eventtime(z = test.data1,
                                     events = test.eventslist3,
                                     width = 2))



### Testing for intraday functionality
  message("Testing for intra-day functionality")
  esConvertNormal5 <- phys2eventtime(test.data.intraday,
                                     test.eventslist.intraday,
                                     3)
  message("Testing for announcement post trading day")
                                        # Altering zoo object
  test.data.intraday2 <- test.data.intraday
  index(test.data.intraday2) <- index(test.data.intraday2) + (3600*24)
  test.data.intraday2 <- rbind(test.data.intraday, test.data.intraday2)
                                        # Altering events list
  test.eventslist.intraday2 <- test.eventslist.intraday
  test.eventslist.intraday2[1, 2] <- as.POSIXct("2010-01-01 18:00:00")
  esConvertNormal6 <- phys2eventtime(test.data.intraday2,
                                     test.eventslist.intraday2,
                                     3)
})




