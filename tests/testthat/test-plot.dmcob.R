context("plot.dmcob")

test_that("plot.dmcob", {

  # test 1: real datasets
  # just check code does not error
  testthat::expect_error(plot(DMCfun::flankerData), NA)
  testthat::expect_error(plot(DMCfun::flankerData, errorBars = FALSE), NA)
  testthat::expect_error(plot(DMCfun::simonData, errorBars = FALSE), NA)
  testthat::expect_error(plot(DMCfun::simonData, errorBarType = "sd"), NA)
  testthat::expect_error(plot(DMCfun::simonData, errorBarType = "se"), NA)
  testthat::expect_error(plot(DMCfun::simonData), NA)

  # test 2: simulated datasets
  dat <- createDF(nVP = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp_comp"   = c(420, 100, 80),
                             "Comp_incomp" = c(470, 100, 95)),
                   Error = list("Comp_comp"   = c(5, 3, 2, 1, 2),
                                "Comp_incomp" = c(15, 8, 4, 2, 2)))
  datOb <- dmcObservedData(dat)

  testthat::expect_error(plot(datOb), NA)
  testthat::expect_error(plot(datOb, errorBars = FALSE), NA)
  testthat::expect_error(plot(datOb, errorBarType = "sd"), NA)
  testthat::expect_error(plot(datOb, errorBarType = "se"), NA)

})
