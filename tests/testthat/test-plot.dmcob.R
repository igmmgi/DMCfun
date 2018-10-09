context("plot.dmcob")

test_that("plot.dmcob", {

  # test 1: real datasets
  # just check code does not error
  testthat::expect_error(plot(DMCfun::flankerData1), NA)
  testthat::expect_error(plot(DMCfun::flankerData2, errorBars = FALSE), NA)
  testthat::expect_error(plot(DMCfun::simonData1, errorBars = FALSE), NA)
  testthat::expect_error(plot(DMCfun::simonData1, errorBarType = "sd"), NA)
  testthat::expect_error(plot(DMCfun::simonData1, errorBarType = "se"), NA)
  testthat::expect_error(plot(DMCfun::simonData2), NA)

  # test 2: simulated datasets
  dat <- createDF(nVP = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list(list(c("Comp:comp"), vals = c(420, 100, 80)),
                             list(c("Comp:incomp"), vals = c(470, 100, 95))),
                   Error = list(list(c("Comp:comp"), vals = c(5, 3, 2, 1, 2)),
                                list(c("Comp:incomp"), vals = c(15, 8, 4, 2, 2))))
  datOb <- dmcObservedData(dat)

  testthat::expect_error(plot(datOb), NA)
  testthat::expect_error(plot(datOb, errorBars = FALSE), NA)
  testthat::expect_error(plot(datOb, errorBarType = "sd"), NA)
  testthat::expect_error(plot(datOb, errorBarType = "se"), NA)

})
