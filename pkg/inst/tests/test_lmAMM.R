context("AMM")

test_that("test.AMM", {
  load(system.file("data", "lmAMMData.rda", package = "eventstudies"))

  firm.returns  <- lmAMMData$Infosys
  market.returns <- lmAMMData$index.nifty
  inrusd <- lmAMMData$currency.inrusd
  rM3 <- lmAMMData$call.money.rate

  cat("\nDoing Testcase P2\n")
  X <- makeX(market.returns, others=inrusd,
             switch.to.innov=FALSE, market.returns.purge=FALSE, verbose=FALSE)
  a <- lmAMM(firm.returns, X, nlags=0, verbose=FALSE)
  expect_that(c(a$exposures, a$s.exposures),
              equals(structure(c(0.7064442,0.3585404,
                                 0.0966792,0.1062146),
                               .Names = c("market.returns", "z",
                                 "market.returns", "z")), tolerance=1e-1))

  cat("\nDoing Testcase P3\n")
  X <- makeX(market.returns, others=inrusd,
             switch.to.innov=TRUE, market.returns.purge=FALSE, verbose=FALSE)
  a <- lmAMM(firm.returns, X, nlags=0, verbose=FALSE)
  expect_that(c(a$exposures, a$s.exposures),
              equals(structure(c(0.67706599, 0.27939607,
                                 0.09719514, 0.10192811),
                               .Names = c("market.returns", "z",
                                 "market.returns", "z")), tolerance=1e-1))
  
  cat("\nDoing Testcase P4\n")
  a <- lmAMM(firm.returns, X, nlags=1, verbose=FALSE)
  expect_that(c(a$exposures, a$s.exposures),
              equals(structure(c(0.68343189, 0.61069556,
                                 0.09794233, 0.12353826),
                               .Names = c("market.returns","z",
                                 "market.returns", "z")),tolerance=1e-1))

  
  cat("\nDoing Testcase P5\n")
  X <- makeX(market.returns, others=inrusd,
             switch.to.innov=TRUE, market.returns.purge=TRUE, nlags=1, verbose=FALSE)
  a <- lmAMM(firm.returns, X, nlags=1, verbose=FALSE)
  expect_that(c(a$exposures, a$s.exposures),
              equals(structure(c(0.68343189, 0.06813816,
                                 0.09844400, 0.11950216),
                               .Names = c("market.returns", "z",
                                 "market.returns", "z")),tolerance=1e-1))
  
  cat("\nDoing Testcase P6\n")
  X <- makeX(market.returns, others=cbind(inrusd, rM3),
             switch.to.innov=c(FALSE, FALSE), market.returns.purge=FALSE, verbose=FALSE)
  a <- lmAMM(firm.returns, X, nlags=0, verbose=FALSE)
  expect_that(c(a$exposures, a$s.exposures),
              equals(structure(c(0.70726513, 0.35942623, -77.52744495,
                                 0.09602279,   0.10668916, 259.10845540),
                               .Names = c("market.returns", "inrusd", "rM3",
                                 "market.returns", "inrusd", "rM3")),
                     tolerance=1e-1))

  cat("\nDoing Testcase P7\n")
  X <- makeX(market.returns, others=cbind(inrusd, rM3),
             switch.to.innov=c(TRUE, TRUE), market.returns.purge=TRUE, nlags=1, verbose=FALSE)
  a <- lmAMM(firm.returns, X, nlags=1, verbose=FALSE)
  
  expect_that(c(a$exposures, a$s.exposures),
              equals(structure(c(6.922458e-01, 6.542345e-02, 1.169788e+03,
                                 1.038158e-01, 1.214853e-01, 5.786265e+02),
                               .Names = c("market.returns", "inrusd", "rM3",
                                 "market.returns", "inrusd", "rM3")),
                     tolerance=1e-1))

################################################################################
                                        #                                                                              #
                                        # THE NEXT CASES TESTS THE FUNCTION FOR THREE COMPANIES FOR THREE YEARS        #
                                        #                                                                              #
################################################################################


  cat("\nDoing Testcases P8\n")
  load(system.file("data", "lmAMMData.rda", package = "eventstudies"))

  nifty <- lmAMMData$index.nifty
  inrusd <- lmAMMData$currency.inrusd
  infosys <- lmAMMData$Infosys
  tcs <- lmAMMData$TCS
  
  regressors <- makeX(nifty, others=inrusd,
                      switch.to.innov=TRUE, market.returns.purge=TRUE, nlags=1,
                      dates=as.Date(c("2012-02-01","2013-01-01","2014-01-20")),
                      verbose=FALSE)

  regressand <- cbind(infosys,tcs)

  res <- manyfirmssubperiod.lmAMM(regressand,regressors,lags=1,
                      dates=as.Date(c("2012-02-01","2013-01-01","2014-01-20")),
                      periodnames=c("P1","P2"),
                      verbose=FALSE)

  match.res <- structure(list(exposures.market.returns.P1=c(0.8446433,0.6875982),
                              exposures.z.P1 = c(-0.05351359,0.36151838),
                              exposures.market.returns.P2=c(0.5865497,0.5822848),
                              exposures.z.P2 = c(0.1375913,-0.0993528),
                              
                              sds.market.returns.P1=c(0.1267067,0.1298345),
                              sds.z.P1 = c(0.1810424, 0.2865279),
                              sds.market.returns.P2=c(0.1353948,0.1111106),
                              sds.z.P2 = c(0.1644340,0.2235453),

                              sig.market.returns.P1=c(6.666130,5.295958),
                              sig.z.P1 = c(-0.295586,1.261721),
                              sig.market.returns.P2=c(4.332144,5.240588),
                              sig.z.P2 = c(0.8367568,-0.4444415)),
                         row.names=c("infosys","tcs"),class="data.frame")
  
  expect_that(as.data.frame(res),
              equals(match.res,check.attributes=FALSE,tolerance=1e-1))
})
