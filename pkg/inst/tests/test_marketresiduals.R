context("Market residuals")

test_that("test.market.residuals", {
library(eventstudies)

load(system.file("data", "mmData.rda", package = "eventstudies"))

mm.formula <- paste("ranbaxyacp","~","nifty",sep="")
mm.result <- marketResidual(mm.formula=mm.formula,data.object=mmData)

# Calculating manually
result <- lm(ranbaxyacp ~ nifty, data=mmData)
resid.res <- xts(result$resid,as.Date(attr(result$resid,"names")))

expect_that(mm.result,equals(resid.res))

})
