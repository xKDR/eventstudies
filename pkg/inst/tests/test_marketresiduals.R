context("Market residuals")

test_that("test.market.residuals", {
library(eventstudies)

load("test_StockPriceReturns.rda")
load("test_NiftyIndex.rda")

alldata <- merge(StockPriceReturns, NiftyIndex, all = TRUE)
StockPriceReturns <- alldata[,-which(colnames(alldata) %in% "NiftyIndex")]
NiftyIndex <- alldata$NiftyIndex

mm.result <- marketResidual(firm.returns=StockPriceReturns[,c("BHEL")],
                            market.returns=NiftyIndex)
mm.result <- xts(mm.result[complete.cases(mm.result),])
colnames(mm.result) <- "BHEL"

# Calculating manually
result <- lm(BHEL ~ NiftyIndex, data=StockPriceReturns)
resid.res <- xts(result$resid,as.Date(attr(result$resid,"names")))
colnames(resid.res) <- "BHEL"

expect_that(mm.result,equals(resid.res))

})
