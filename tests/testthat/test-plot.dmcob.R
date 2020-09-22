context("plot.dmcob")

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
  testthat::expect_error(plot(DMCfun::flankerData, figType = "all"), NA)
  testthat::expect_error(plot(DMCfun::flankerData, figType = "xxx"))
  testthat::expect_error(plot(DMCfun::flankerData, labels = c("a", "b", "c")))
  testthat::expect_error(plot(DMCfun::flankerData, legend = "xxx"))
  testthat::expect_error(plot(DMCfun::flankerData, subject = 1), NA)
  testthat::expect_error(plot(DMCfun::flankerData, subject = 999))
  testthat::expect_error(plot(DMCfun::flankerData, xlabs = FALSE, ylabs = FALSE, xaxts = FALSE, yaxts = FALSE), NA)
  testthat::expect_error(plot(DMCfun::flankerData, legend = function(){}), NA)
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
  testthat::expect_error(plot(DMCfun::simonData, figType = "all"), NA)
  testthat::expect_error(plot(DMCfun::simonData, figType = "xxx"))
  testthat::expect_error(plot(DMCfun::simonData, labels = c("a", "b", "c")))
  testthat::expect_error(plot(DMCfun::simonData, legend = "xxx"))
  testthat::expect_error(plot(DMCfun::simonData, subject = 1), NA)
  testthat::expect_error(plot(DMCfun::simonData, subject = 999))
  testthat::expect_error(plot(DMCfun::simonData, xlabs = FALSE, ylabs = FALSE, xaxts = FALSE, yaxts = FALSE), NA)
  testthat::expect_error(plot(DMCfun::simonData, legend = function(){}), NA)
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

})
