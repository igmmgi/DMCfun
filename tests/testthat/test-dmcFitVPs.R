context("dmcFitVPs")

test_that("dmcFitVPs", {

  # test 1
  fit <- dmcFitVPs(DMCfun::flankerData, nTrl = 1000, VP = c(1, 2, 3),
                   printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfitvp")
  testthat::expect_s3_class(fit[[1]], "dmcfit")

  # test 3
  fit <- dmcFitVPs(DMCfun::simonData, nTrl = 1000, VP = c(1, 3, 5),
                   printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfitvp")
  testthat::expect_s3_class(fit[[1]], "dmcfit")
  testthat::expect_null(fit[[2]])
  testthat::expect_s3_class(fit[[3]], "dmcfit")
  testthat::expect_null(fit[[4]])
  
})
