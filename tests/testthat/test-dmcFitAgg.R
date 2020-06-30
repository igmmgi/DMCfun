context("dmcFitAgg")

test_that("dmcFitAgg", {

  # basic fit of flanker data
  fit <- dmcFitAgg(DMCfun::flankerData, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")

  # basic fit of simon data
  fit <- dmcFitAgg(DMCfun::simonData, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")

  # fit with some non-default starting values and some fixed parameters
  fit <- dmcFitAgg(DMCfun::flankerData,  nTrl = 1000, printInputArgs = FALSE, printResults = FALSE,
                   startVals = list(mu = 0.6, aaShape = 2.5), 
                   fixedFit = list(mu = T, aaShape = T))
 
  testthat::expect_equal(fit$par$mu, 0.6) 
  testthat::expect_equal(fit$par$aaShape, 2.5) 
  
})
