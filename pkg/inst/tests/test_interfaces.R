context("eventstudy")

test_that("test.interfaces", {
    load("test_SplitDates.rda")
    load("test_StockPriceReturns.rda")
    load("test_NiftyIndex.rda")
    load("test_USDINR.rda")

### Basic event study with default args (market residuals)
    cat("\nChecking market residuals interface: ")
    expected_mean <- c(0, 0.0393985717416213, -0.7458035091065,
                     0.457817077869512, 0.715714066835461, 2.33986420702835,
                     2.37333344340029)
    expected_outcomes <- c("success", "success")

    test_events <- data.frame(name = "ONGC",
                              when = as.Date(c("2011-08-01", "2010-05-14")),
                              stringsAsFactors = FALSE)
    test_returns<- StockPriceReturns[complete.cases(StockPriceReturns$ONGC), "ONGC",
                                     drop = FALSE]
    test_es <- eventstudy(firm.returns = test_returns,
                     event.list = test_events,
                     event.window = 3,
                     model.args = list(market.returns = NiftyIndex))

    expect_that(expected_mean, equals(test_es$result[, "Mean"]))
    expect_that(expected_outcomes, equals(test_es$outcomes))
    expect_is(test_es, "es")

### None
    cat("\nChecking no model output: ")
    expected_mean <- c(0, -0.197406699931557, -0.804299958306487,
                       0.0135570496689663, -0.418062964428412,
                       0.904144365357373, -0.806779427723603)
    expected_outcomes <- c("success", "success")

    test_events <- data.frame(name = "ONGC",
                              when = c("2011-08-01", "2010-05-14"),
                              stringsAsFactors = FALSE)
    test_returns<- StockPriceReturns[complete.cases(StockPriceReturns$ONGC), "ONGC",
                                     drop = FALSE]
    test_es <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "None")

    expect_that(expected_mean, equals(test_es$result[, "Mean"]))
    expect_that(expected_outcomes, equals(test_es$outcomes))
    expect_is(test_es, "es")

### AMM interface
    cat("\nChecking AMM interface: ")
    expected_mean <-  c(0, 0.135927645042554, -0.600457594252805, 0.631525565290171,
                        0.871423869901684, 2.54741102266723, 2.5989730099384)

    expected_outcomes <- c("success", "success")

    test_events <- data.frame(name = "ONGC",
                              when = c("2011-08-01", "2010-05-14"),
                              stringsAsFactors = FALSE)
    test_returns<- StockPriceReturns[complete.cases(StockPriceReturns$ONGC), "ONGC",
                                     drop = FALSE]
    test_others <- USDINR
    test_es <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "lmAMM",
                          model.args = list(market.returns = NiftyIndex[index(USDINR)],
                          others = test_others))

    expect_that(expected_mean, equals(test_es$result[, "Mean"]))
    expect_that(expected_outcomes, equals(test_es$outcomes))
    expect_is(test_es, "es")

### Excess return
    cat("\nChecking excess return interface: ")
    expected_mean <- c(0, 0.138567158395153, -0.631185954448288, 0.701644918222266,
                       1.15001275036422, 2.88646832315114, 3.32315429568726)
    expected_outcomes <- c("success", "success")

    test_events <- data.frame(name = "ONGC",
                              when = c("2011-08-01", "2010-05-14"),
                              stringsAsFactors = FALSE)
    test_returns<- StockPriceReturns[complete.cases(StockPriceReturns$ONGC), "ONGC",
                                     drop = FALSE]

    test_es <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "excessReturn",
                          model.args = list(market.returns = NiftyIndex))

    expect_that(expected_mean, equals(test_es$result[, "Mean"]))
    expect_that(expected_outcomes, equals(test_es$outcomes))
    expect_is(test_es, "es")

### Remapping
    cat("\nChecking remapping: ")
    test_events <- data.frame(name = "ONGC",
                              when = c("2011-08-01", "2010-05-14"),
                              stringsAsFactors = FALSE)
    test_returns <- StockPriceReturns[complete.cases(StockPriceReturns$ONGC), "ONGC",
                                      drop = FALSE]

    ## cumsum
    test_es <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "None",
                          to.remap = FALSE,
                          remap = "cumsum")

    test_es_remap <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "None",
                          to.remap = TRUE,
                          remap = "cumsum")

    expect_false(isTRUE(all.equal(test_es, test_es_remap)))

    ## cumprod
    test_es <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "None",
                          to.remap = FALSE,
                          remap = "cumprod")

    test_es_remap <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "None",
                          to.remap = TRUE,
                          remap = "cumprod")

    expect_false(isTRUE(all.equal(test_es, test_es_remap)))

### Inference
    cat("\nChecking inference interface: ")
    ## bootstrap
    test_es_inference <- eventstudy(firm.returns = test_returns,
                                    event.list = test_events,
                                    event.window = 3,
                                    type = "None",
                                    inference = TRUE,
                                    inference.strategy = "bootstrap")

    test_es <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "None",
                          inference = FALSE,
                          inference.strategy = "bootstrap")

    expect_false(isTRUE(all.equal(test_es, test_es_inference)))

    ## wilcoxon
    test_es_inference <- eventstudy(firm.returns = test_returns,
                                    event.list = test_events,
                                    event.window = 3,
                                    type = "None",
                                    inference = TRUE,
                                    inference.strategy = "wilcoxon")

    test_es <- eventstudy(firm.returns = test_returns,
                          event.list = test_events,
                          event.window = 3,
                          type = "None",
                          inference = FALSE,
                          inference.strategy = "wilcoxon")

    expect_false(isTRUE(all.equal(test_es, test_es_inference)))

})                                       # end test_that()

test_that("test.arguments", {
    load("test_StockPriceReturns.rda")

    cat("\nChecking single series handling: ")
    test_events <- data.frame(name = "ONGC",
                              when = c("2011-08-01", "2010-05-14"),
                              stringsAsFactors = FALSE)
    test_returns<- StockPriceReturns$ONGC
    expect_error(eventstudy(firm.returns = test_returns,
                            event.list = test_events,
                            event.window = 3,
                            type = "None"))
})
