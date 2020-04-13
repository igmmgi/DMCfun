context("errDist")

test_that("errDist", {

  set.seed(1)
  
  # test 1: 10% errors
  errs <- errDist(10000, 10)

  testthat::expect_equal(length(errs), 10000)
  testthat::expect_equal(0.1, round(mean(errs), 1))

  # test 2: 20% errors 
  errs <- errDist(10000, 20)

  testthat::expect_equal(length(errs), 10000)
  testthat::expect_equal(0.2, round(mean(errs), 1))

})
