context("plot.dmcsim")

test_that("plot.dmcsim", {

  # test 1
  dmc <- dmcSim(fullData = TRUE, printInputArgs = FALSE, printResults = FALSE)

  # just check code does not error
  testthat::expect_error(plot(dmc, figType = "summary1"), NA)
  testthat::expect_error(plot(dmc, figType = "summary2"), NA)
  testthat::expect_error(plot(dmc, figType = "activation"), NA)
  testthat::expect_error(plot(dmc, figType = "trials"), NA)
  testthat::expect_error(plot(dmc, figType = "pdf"), NA)
  testthat::expect_error(plot(dmc, figType = "cdf"), NA)
  testthat::expect_error(plot(dmc, figType = "caf"), NA)
  testthat::expect_error(plot(dmc, figType = "delta"), NA)
  testthat::expect_error(plot(dmc, figType = "rtCorrect"), NA)
  testthat::expect_error(plot(dmc, figType = "rtErrors"), NA)
  testthat::expect_error(plot(dmc, figType = "errorRate"), NA)
  testthat::expect_error(plot(dmc, figType = "all"), NA)

  # test 2
  dmc <- dmcSim(fullData = FALSE, printInputArgs = FALSE, printResults = FALSE)

  # just check code does not error
  testthat::expect_error(plot(dmc, figType = "summary2"), NA)
  testthat::expect_error(plot(dmc, figType = "cdf"), NA)
  testthat::expect_error(plot(dmc, figType = "caf"), NA)
  testthat::expect_error(plot(dmc, figType = "delta"), NA)
  testthat::expect_error(plot(dmc, figType = "rtCorrect"), NA)
  testthat::expect_error(plot(dmc, figType = "rtErrors"), NA)
  testthat::expect_error(plot(dmc, figType = "errorRate"), NA)
  testthat::expect_error(plot(dmc, figType = "all"), NA)

})
