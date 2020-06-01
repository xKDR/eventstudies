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
  test.eventslist <- data.frame(name = c("ITC","Reliance","TCS",
                                  "ITC","Reliance","Junk"),
                                when = as.Date(c("2004-01-02",
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

  x <- structure(c(0, -0.00883936242455352, -0.0221156214636737,
                   -0.00866697835241492, 0.00833834491715368,
                   -0.0112558648518828, -0.0241267269375545,
                   0.00375234961855053, 0.019963444246633),
                 index = -4:4,
                 class = "zoo")
  expect_equal(x, es.test.remap0)

  y <- structure(c(0, 0.0278562751459042, 0.0644716084647099, 0,
                   0.0170053232695686, -0.00258888649946787, 0,
                   -0.0153232381366424, -0.0408950663236913 ),
                 .Dim = c(3L, 3L),
                 .Dimnames = list(NULL, c("1", "2", "3")),
                 index = -1:1,
                 class = "zoo")
  expect_equal(y, es.test.remap1)

  ### Testing remap.cumprod for univariate series i.e. only one firm

  cat("\nTesting for univariate series in remap.cumprod")
  es.test.remap2 <- remap.cumprod(es.test.w0, is.pc = FALSE,
                                  base = 1)
  es.test.remap3 <- remap.cumprod(es.test.w1, is.pc = FALSE,
                                  base = 1)

  x <- structure(c(1, 0.991160637575446, 0.978001732201615, 0.991154528460188,
                   1.00800943162675, 0.988258283374288, 0.975538547303955,
                   1.00273566114767, 1.01899110383749),
                 index = -4:4,
                 class = "zoo")
  expect_equal(x, es.test.remap2)

  y  <- structure(c(1, 1.0278562751459, 1.0654915752642, 1, 1.01700532326957,
                    0.997077907629198, 1, 0.984676761863358, 0.959496776889208),
                  .Dim = c(3L, 3L),
                  .Dimnames = list(NULL, c("1", "2", "3")),
                  index = -1:1,
                  class = "zoo")
  expect_equal(y, es.test.remap3)

  ### Testing remap.cumsum for percentage and absolute returns

  cat("\nTesting for percentage and absolute returns in remap.cumsum")

  es.test.remap4 <- remap.cumsum(es.test.w0, is.pc = TRUE, base = 0)

  es.test.remap5 <- remap.cumsum(es.test.w0, is.pc = FALSE, base = 0)

  x <- structure(c(0, -0.0000883936242455352, -0.000221156214636737,
                   -0.0000866697835241492, 0.0000833834491715368, -0.000112558648518828,
                   -0.000241267269375545, 0.0000375234961855053, 0.00019963444246633
                   ), index = -4:4, class = "zoo")
  expect_equal(x, es.test.remap4)

  y <- structure(c(0, -0.00883936242455352, -0.0221156214636737, -0.00866697835241492,
                   0.00833834491715368, -0.0112558648518828, -0.0241267269375545,
                   0.00375234961855053, 0.019963444246633), index = -4:4, class = "zoo")
  expect_equal(y, es.test.remap5)

  ### Testing remap.cumprod for percentage and absolute returns

  cat("\nTesting for percentage and absolute returns in remap.cumprod")

  es.test.remap6 <- remap.cumsum(es.test.w0, is.pc = TRUE, base = 100)

  es.test.remap7 <- remap.cumsum(es.test.w0, is.pc = FALSE, base = 100)

  x <- structure(c(100, 99.9999116063758, 99.9997788437854, 99.9999133302165,
                   100.000083383449, 99.9998874413515, 99.9997587327306,
                   100.000037523496, 100.000199634442), index = -4:4, class = "zoo")
  expect_equal(x, es.test.remap6)

  y <- structure(c(100, 99.9911606375754, 99.9778843785363, 99.9913330216476,
                   100.008338344917, 99.9887441351481, 99.9758732730624,
                   100.003752349619, 100.019963444247), index = -4:4, class = "zoo")
  expect_equal(y, es.test.remap7)
})
