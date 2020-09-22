context("dmcFitSubject")

test_that("dmcFitSubject", {

  # test 1
  fit <- dmcFitSubject(DMCfun::flankerData, nTrl = 1000, subjects = c(1),
                       printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")
  testthat::expect_s3_class(fit[[1]], "dmcfit")

  # test 2
  fit <- dmcFitSubject(DMCfun::simonData, nTrl = 1000, subjects = c(5),
                       printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_null(fit[[1]])
  testthat::expect_s3_class(fit[[5]], "dmcfit")
  
})
