context("plot")

test_that("plot.dmcsim", {

  # test 1
  dmc <- dmcSim(fullData = TRUE, deltaErrors = TRUE, printInputArgs = FALSE, printResults = FALSE)

  # should give error
  testthat::expect_error(plot(dmc, figType = "xxx"))

  # just check code does not error
  testthat::expect_error(plot(dmc, figType = "summary1"), NA)
  testthat::expect_error(plot(dmc, figType = "summary2"), NA)
  testthat::expect_error(plot(dmc, figType = "summary3"), NA)
  testthat::expect_error(plot(dmc, figType = "activation"), NA)
  testthat::expect_error(plot(dmc, figType = "trials"), NA)
  testthat::expect_error(plot(dmc, figType = "pdf"), NA)
  testthat::expect_error(plot(dmc, figType = "cdf"), NA)
  testthat::expect_error(plot(dmc, figType = "caf"), NA)
  testthat::expect_error(plot(dmc, figType = "delta"), NA)
  testthat::expect_error(plot(dmc, figType = "rtCorrect", errorBars = TRUE), NA)
  testthat::expect_error(plot(dmc, figType = "rtErrors"), NA)
  testthat::expect_error(plot(dmc, figType = "errorRate"), NA)
  testthat::expect_error(plot(dmc, figType = "all"), NA)

  # test 2
  dmc <- dmcSim(fullData = FALSE, printInputArgs = FALSE, printResults = FALSE)

  # should give error
  testthat::expect_error(plot(dmc, figType = "activation"))
  testthat::expect_error(plot(dmc, labels = c("a", "b", "c")))

  # just check code does not error
  testthat::expect_error(plot(dmc, figType = "summary1"), NA)
  testthat::expect_error(plot(dmc, figType = "summary2"), NA)
  testthat::expect_error(plot(dmc, figType = "cdf"), NA)
  testthat::expect_error(plot(dmc, figType = "caf"), NA)
  testthat::expect_error(plot(dmc, figType = "delta"), NA)
  testthat::expect_error(plot(dmc, figType = "rtCorrect"), NA)
  testthat::expect_error(plot(dmc, figType = "rtErrors"), NA)
  testthat::expect_error(plot(dmc, figType = "errorRate"), NA)
  testthat::expect_error(plot(dmc, figType = "all"), NA)

  # test 3
  params <- list(amp = seq(20, 30, 2))
  dmc <- dmcSims(params)
  plot(dmc, col = c("red", "green"))

  # test 4
  params <- list(amp = seq(20, 30, 2))
  dmc <- dmcSims(params)
  plot(dmc, col = c("red", "green"), legend.parameters = list(ncol=2))


})

test_that("plot.dmcob", {

  # real datasets
  testthat::expect_error(plot(DMCfun::flankerData), NA)
  testthat::expect_error(plot(DMCfun::flankerData, errorBars = TRUE), NA)
  testthat::expect_error(plot(DMCfun::flankerData, errorBars = TRUE, errorBarType = "sd"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, errorBars = TRUE, errorBarType = "xxx"))
  testthat::expect_error(plot(DMCfun::flankerData, figType = "summary"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, figType = "rtCorrect"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, figType = "errorRate"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, figType = "rtErrors"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, figType = "cdf"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, figType = "caf"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, figType = "delta"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, figType = "xxx"))
  testthat::expect_error(plot(DMCfun::flankerData, labels = c("a", "b", "c")))
  testthat::expect_error(plot(DMCfun::flankerData, legend = "xxx"))
  testthat::expect_error(plot(DMCfun::flankerData, subject = 1), NA)
  testthat::expect_error(plot(DMCfun::flankerData, subject = 999))
  testthat::expect_error(plot(DMCfun::flankerData, xlabs = FALSE, ylabs = FALSE, xaxts = FALSE, yaxts = FALSE), NA)
  testthat::expect_error(plot(DMCfun::flankerData, cafBinLabels = TRUE), NA)
  testthat::expect_error(plot(DMCfun::simonData), NA)
  testthat::expect_error(plot(DMCfun::simonData, errorBars = TRUE), NA)
  testthat::expect_error(plot(DMCfun::simonData, errorBars = TRUE, errorBarType = "sd"), NA)
  testthat::expect_error(plot(DMCfun::simonData, errorBars = TRUE, errorBarType = "xxx"))
  testthat::expect_error(plot(DMCfun::simonData, figType = "summary"), NA)
  testthat::expect_error(plot(DMCfun::simonData, figType = "rtCorrect"), NA)
  testthat::expect_error(plot(DMCfun::simonData, figType = "errorRate"), NA)
  testthat::expect_error(plot(DMCfun::simonData, figType = "rtErrors"), NA)
  testthat::expect_error(plot(DMCfun::simonData, figType = "cdf"), NA)
  testthat::expect_error(plot(DMCfun::simonData, figType = "caf"), NA)
  testthat::expect_error(plot(DMCfun::simonData, figType = "delta"), NA)
  testthat::expect_error(plot(DMCfun::simonData, figType = "xxx"))
  testthat::expect_error(plot(DMCfun::simonData, labels = c("a", "b", "c")))
  testthat::expect_error(plot(DMCfun::simonData, legend = "xxx"))
  testthat::expect_error(plot(DMCfun::simonData, subject = 1), NA)
  testthat::expect_error(plot(DMCfun::simonData, subject = 999))
  testthat::expect_error(plot(DMCfun::simonData, xlabs = FALSE, ylabs = FALSE, xaxts = FALSE, yaxts = FALSE), NA)
  testthat::expect_error(plot(DMCfun::simonData, cafBinLabels = TRUE), NA)

  # simulated datasets
  dat <- createDF(nSubjects = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp_comp"   = c(420, 100, 80),
                             "Comp_incomp" = c(470, 100, 95)),
                   Error = list("Comp_comp"   = c(5, 3, 2, 1, 2),
                                "Comp_incomp" = c(15, 8, 4, 2, 2)))
  datOb <- dmcObservedData(dat)
  testthat::expect_error(plot(datOb), NA)

  # plot combined data
  dat <- dmcCombineObservedData(DMCfun::flankerData, DMCfun::simonData)  # combine flanker/simon data
  testthat::expect_error(plot(dat, figType = "delta",  cols = c("black", "darkgrey"),
                              pchs = c(1, 2), resetPar = TRUE), NA)

})

test_that("plot.dmcfit", {

  # just check code does not error
  # test 1
  resTh <- dmcFit(DMCfun::flankerData, nTrl = 1000,
                  printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData), NA)

  # test 2
  resTh <- dmcFit(DMCfun::simonData, nTrl = 1000,
                  printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::simonData), NA)

  # test 3
  resTh <- dmcFitSubject(DMCfun::flankerData, nTrl = 1000, subjects = 10,
                         printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData, subject = 10), NA)

  # test 4
  dat <- createDF(nSubjects = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp_comp"   = c(500, 150, 100),
                             "Comp_incomp" = c(530, 150, 150)),
                   Error = list("Comp_comp"   = c(5, 3, 2, 1, 1),
                                "Comp_incomp" = c(15, 12, 5, 2, 1)))
  datOb <- dmcObservedData(dat)

  resTh <- dmcFit(datOb, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, datOb), NA)

})
