context("dmcFitAgg")

test_that("dmcFitAgg", {

  fit <- dmcFitAgg(DMCfun::flankerData, nTrl = 10000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit[[1]], "dmcsim")

  fit <- dmcFitAgg(DMCfun::flankerData, nTrl = 10000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit[[1]], "dmcsim")

  fit <- dmcFitAgg(DMCfun::simonData, nTrl = 10000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit[[1]], "dmcsim")

  fit <- dmcFitAgg(DMCfun::flankerData, nTrl = 10000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit[[1]], "dmcsim")

  fit <- dmcFitAgg(DMCfun::flankerData, nTrl = 10000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit[[1]], "dmcsim")

})
