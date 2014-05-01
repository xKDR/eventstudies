context("eventstudy")

test_that("test.interfaces", {
    load("test_SplitDates.rda")
    load("test_StockPriceReturns.rda")
    load("test_NiftyIndex.rda")


### Basic event study with default args (market residuals)
    cat("Checking market residuals interface: ")
    expected_mean <- c(0, 0.0393985717416213, -0.7458035091065,
                     0.457817077869512, 0.715714066835461, 2.33986420702835,
                     2.37333344340029)
    expected_outcomes <- c("success", "success")

    test_events <- data.frame(outcome.unit = "ONGC",
                              event.when = c("2011-08-01", "2010-05-14"),
                              stringsAsFactors = FALSE)
    test_returns<- StockPriceReturns[complete.cases(StockPriceReturns$ONGC), "ONGC",
                                     drop = FALSE]
    test_es <- eventstudy(firm.returns = test_returns,
                     eventList = test_events,
                     width = 3,
                     market.returns = NiftyIndex)

    expect_that(expected_mean, equals(test_es$eventstudy.output[, "Mean"]))
    expect_that(expected_outcomes, equals(test_es$outcomes))
    expect_is(test_es, "es")

### AMM interface
    cat("Checking AMM interface: ")
XX    expected_mean <- c(0, 0.0393985717416213, -0.7458035091065,
                     0.457817077869512, 0.715714066835461, 2.33986420702835,
                     2.37333344340029)
    expected_outcomes <- c("success", "success")

    test_events <- data.frame(outcome.unit = "ONGC",
                              event.when = c("2011-08-01", "2010-05-14"),
                              stringsAsFactors = FALSE)
    test_returns<- StockPriceReturns[complete.cases(StockPriceReturns$ONGC), "ONGC",
                                     drop = FALSE]
    test_es <- eventstudy(firm.returns = test_returns,
                          eventList = test_events,
                          width = 3,
                          type = "lmAMM",
                          market.returns = NiftyIndex,
                          others = test_others)

    expect_that(expected_mean, equals(test_es$eventstudy.output[, "Mean"]))
    expect_that(expected_outcomes, equals(test_es$outcomes))
    expect_is(test_es, "es")
