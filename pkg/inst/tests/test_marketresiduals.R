context("Market residuals")

test_that("test.market.residuals", {
library(eventstudies)

load(system.file("data", "StockPriceReturns.rda", package = "eventstudies"))

mm.result <- marketResidual(data.object=StockPriceReturns[,c("BHEL","nifty")],
                            market.name="nifty")
mm.result <- mm.result[complete.cases(mm.result),]

# Calculating manually
result <- lm(BHEL ~ nifty, data=StockPriceReturns)
resid.res <- xts(result$resid,as.Date(attr(result$resid,"names")))
colnames(resid.res) <- "BHEL"

expect_that(mm.result,equals(resid.res))

})
