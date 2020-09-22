context("dmcFitAgg")

test_that("dmcFitAgg", {

  # basic fit of flanker data
  fit <- dmcFit(DMCfun::flankerData, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")

  # basic fit of simon data
  fit <- dmcFit(DMCfun::simonData, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")

  # fit with some non-default starting values and some fixed parameters
  fit <- dmcFit(DMCfun::flankerData,  nTrl = 1000, printInputArgs = FALSE, printResults = FALSE,
                startVals = list(drc = 0.6, aaShape = 2.5), 
                fixedFit = list(drc = T, aaShape = T))
 
  testthat::expect_equal(fit$par$drc, 0.6) 
  testthat::expect_equal(fit$par$aaShape, 2.5) 
  
})
