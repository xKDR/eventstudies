context("INR Inference")

test_that("test.inr.inference", {
library(eventstudies)

load(system.file("data", "inr.rda",package = "eventstudies"))

inr_returns <- diff(log(inr))[-1]

eventslist <- data.frame(outcome.unit=rep("inr",10),
                         event.when=as.Date(c(
                           "2010-04-20","2010-07-02","2010-07-27",
                           "2010-09-16","2010-11-02","2011-01-25",
                           "2011-03-17","2011-05-03","2011-06-16",
                           "2011-07-26")
                           )
                         )

event_time_data <- phys2eventtime(inr_returns,eventslist,width=10)
w <- window(event_time_data$z.e,start=-10,end=10)

expect_that(inference.bootstrap(w, to.plot=FALSE)[,2],
            equals(c(-0.000015327455,
                     -0.002526819039,
                     0.001190000495,
                     0.001193534934,
                     0.001846733711,
                     -0.000105473215,
                     -0.001659771669,
                     0.001644517771,
                     -0.001325235918,
                     0.001546368579,
                     -0.000809734240,
                     -0.001499191073,
                     -0.000289413740,
                     -0.000003273428,
                     -0.000416661873,
                     -0.001150000190,
                     -0.000759748390,
                     0.002306711019,
                     -0.0004872993296,
                     0.001122457470,
                     0.000635889955)))
})

