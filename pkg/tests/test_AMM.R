test.AMM <- function() {
  library(eventstudies)
  library(sandwich)

  data("firmExposures",package="eventstudies")

  rj  <- firmExposures$Company_A
  rM1 <- firmExposures$NIFTY_INDEX
  rM2 <- firmExposures$usdinr
  rM3 <- firmExposures$baa

  cat("Doing Testcase P2:\n")
  X <- makeX(rM1, others=rM2,
             switch.to.innov=FALSE, rM1purge=FALSE, verbose=FALSE)
  a <- firmExposures(rj, X, nlags=0, verbose=FALSE)
  test.result <- all.equal(c(a$exposures, a$s.exposures),
            structure(c(0.716160223601197,-0.673093436292401,
                        0.152101606133946,1.02143820457251),
                      .Names = c("rM1", "z", "rM1", "z")), tolerance=1e-1)
  checkTrue(isTRUE(test.result))

  cat("Doing Testcase P3:\n")
  X <- makeX(rM1, others=rM2,
             switch.to.innov=TRUE, rM1purge=FALSE, verbose=FALSE)
  a <- firmExposures(rj, X, nlags=0, verbose=FALSE)
  test.result <- all.equal(c(a$exposures, a$s.exposures),
            structure(c(0.716160223601197,-0.673093436292401,
                        0.152100337597009,1.02146106755333),
                      .Names = c("rM1", "z", "rM1", "z")), tolerance=1e-1)
  checkTrue(isTRUE(test.result))
  
  cat("Doing Testcase P4:\n")
  a <- firmExposures(rj, X, nlags=1, verbose=FALSE)
  test.result <- all.equal(c(a$exposures, a$s.exposures),
            structure(c( 0.736264286484902, -1.450805,
                        0.177929844631439, 1.646730),
                      .Names = c("rM1","z", "rM1", "z")),tolerance=1e-1)
  checkTrue(isTRUE(test.result))
  
  cat("Doing Testcase P5:\n")
  X <- makeX(rM1, others=rM2,
             switch.to.innov=TRUE, rM1purge=TRUE, nlags=1, verbose=FALSE)
  a <- firmExposures(rj, X, nlags=1, verbose=FALSE)
  test.result <- all.equal(c(a$exposures, a$s.exposures),
            structure(c(0.7365566,-2.340171,
                        0.1653025, 1.1436666),
                      .Names = c("rM1", "z", "rM1", "z")),tolerance=1e-1)
  checkTrue(isTRUE(test.result))
  
  cat("Doing Testcase P6:\n")
  X <- makeX(rM1, others=cbind(rM2, rM3),
             switch.to.innov=c(FALSE, FALSE), rM1purge=FALSE, verbose=FALSE)
  a <- firmExposures(rj, X, nlags=0, verbose=FALSE)
  test.result <-   all.equal(c(a$exposures, a$s.exposures),
            structure(c(0.7230599,-0.7642377,
                        0.207374104922771,0.173380799334299,
                        1.01806122963342,0.467821650129292),
                      .Names = c("rM1", "rM2", "rM3", "rM1", "rM2", "rM3")),tolerance=1e-1)
  checkTrue(isTRUE(test.result))
  
  cat("Doing Testcase P7:\n")
  X <- makeX(rM1, others=cbind(rM2, rM3),
             switch.to.innov=c(TRUE, TRUE), rM1purge=TRUE, nlags=1, verbose=FALSE)
  a <- firmExposures(rj, X, nlags=1, verbose=FALSE)
  test.result <- all.equal(c(a$exposures, a$s.exposures),
            structure(c(0.7482719,-1.9468851,-0.4802211,
                        0.1740678,1.2455112,0.6146619),
                      .Names = c("rM1", "rM2", "rM3", "rM1", "rM2", "rM3")),tolerance=1e-1)
  checkTrue(isTRUE(test.result))

################################################################################
                                        #                                                                              #
                                        # THE NEXT CASES TESTS THE FUNCTION FOR THREE COMPANIES FOR THREE YEARS        #
                                        #                                                                              #
################################################################################


  cat("Doing Testcases P8:\n")
  data("y3c3", package="eventstudies")

  NIFTY_INDEX <- y3c3$NIFTY_INDEX
  INRUSD <- y3c3$INRUSD
  Company_A <- y3c3$Company_A
  Company_B <- y3c3$Company_B
  Company_C <- y3c3$Company_C

  regressors <- makeX(NIFTY_INDEX, others=INRUSD,
                      switch.to.innov=TRUE, rM1purge=TRUE, nlags=1,
                      dates=as.Date(c("2005-01-15","2006-01-07","2007-01-06",
                        "2008-01-05","2009-01-03")), verbose=FALSE)

  regressand <- cbind(Company_A,Company_B,Company_C)

  res <- manyfirmsAMM(regressand,regressors,lags=1,
                       dates=as.Date(c("2005-01-15","2006-01-07","2007-01-06",
                         "2008-01-05","2009-01-03")),periodnames=c("P1","P2","P3","P4"),
                       verbose=FALSE)

  test.result <- all.equal(as.data.frame(res),structure(list(rM1.P1 = c(0.756294904326272, 0.359467326140834,0.914021428042946), z.P1 = c(-2.23264294525560, -1.05654919420689,0.296635483126946), rM1.P2 = c(1.02094561445355, 0.988758963378838,0.879236409569888), z.P2 = c(-4.72831391695047, -2.0508684999854,-1.02215809586573), rM1.P3 = c(1.20585808099744, 0.676388278572118,0.530718379431386), z.P3 = c(-1.32677083522489, -2.74055730512260, -1.50032216697694), rM1.P4 = c(1.11331096371784, 0.437117737120777,0.663182186702262), z.P4 = c(-2.05336868436562, -1.60350865767951,-0.466253391408585), rM1.P1 = c(0.143617135793294, 0.263130891045529,0.154272220123111), z.P1 = c(1.20226371286803, 1.22122136357895,1.02442932195400), rM1.P2 = c(0.203037609116444, 0.123122376136099,0.121880488983820), z.P2 = c(1.118400430819, 0.798694545623495,1.29755067543957), rM1.P3 = c(0.230304109532112, 0.289262660515515,0.164866239494693), z.P3 = c(1.17618117392934, 0.795008683829453,0.650736332270758), rM1.P4 = c(0.231338818884745, 0.213858364836974,0.207154237634752), z.P4 = c(0.771450066857429, 0.415931231130697,0.696448914066602), rM1.P1 = c(5.26604920888266, 1.36611602200152,5.9247311493511), z.P1 = c(-1.85703263049467, -0.865157804896683,0.289561687438957), rM1.P2 = c(5.02835715460001, 8.0307007906172,7.21392256382075), z.P2 = c(-4.2277468665565, -2.56777576762391,-0.787759673062059), rM1.P3 = c(5.23593818385294, 2.33831866638673,3.21908464133114), z.P3 = c(-1.12803270842405, -3.44720423923131,-2.30557614900882), rM1.P4 = c(4.81246929972659, 2.04395903547657,3.20139329165723), z.P4 = c(-2.66170005367969, -3.85522542589652,-0.669472493949494)), .Names = c("rM1.P1", "z.P1", "rM1.P2","z.P2", "rM1.P3", "z.P3", "rM1.P4", "z.P4", "rM1.P1", "z.P1","rM1.P2", "z.P2", "rM1.P3", "z.P3", "rM1.P4", "z.P4", "rM1.P1", "z.P1", "rM1.P2", "z.P2", "rM1.P3", "z.P3", "rM1.P4", "z.P4"),row.names = c("Company_A","Company_B", "Company_C"), class = "data.frame"),check.attributes=FALSE)

  checkTrue(isTRUE(test.result))
}
