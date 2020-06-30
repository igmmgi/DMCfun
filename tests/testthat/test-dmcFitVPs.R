context("dmcFitVPs")

test_that("dmcFitVPs", {

  # test 1
  fit <- dmcFitVPs(DMCfun::flankerData, nTrl = 1000, VP = c(1),
                   printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfitvp")
  testthat::expect_s3_class(fit[[1]], "dmcfit")

  # test 2
  fit <- dmcFitVPs(DMCfun::simonData, nTrl = 1000, VP = c(5),
                   printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_null(fit[[1]])
  testthat::expect_s3_class(fit[[5]], "dmcfit")
  
})
