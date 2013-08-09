context("Market residuals")

test_that("test.market.residuals", {
library(eventstudies)

load(system.file("data", "StockPriceReturns.rda", package = "eventstudies"))
load(system.file("data", "nifty.index.rda", package = "eventstudies"))

mm.result <- marketResidual(firm.returns=StockPriceReturns[,c("BHEL")],
                            market.returns=nifty.index)
mm.result <- xts(mm.result[complete.cases(mm.result),])
colnames(mm.result) <- "BHEL"

# Calculating manually
result <- lm(BHEL ~ nifty, data=StockPriceReturns)
resid.res <- xts(result$resid,as.Date(attr(result$resid,"names")))
colnames(resid.res) <- "BHEL"

expect_that(mm.result,equals(resid.res))

})
