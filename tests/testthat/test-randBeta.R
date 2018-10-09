context("randBeta")

test_that("randBeta", {

  # test 1
  set.seed(1)
  x <- DMCfun::randBeta()
  testthat::expect_equal(length(x), 1)

  # test 2
  set.seed(1)
  x <- DMCfun::randBeta(n = 100)
  testthat::expect_equal(length(x), 100)
  testthat::expect_equal(0.5, round(mean(x), 1))

  # test 3
  set.seed(1)
  x <- DMCfun::randBeta(n = 10000, shape = 2)
  testthat::expect_equal(length(x), 10000)
  testthat::expect_equal(0.5, round(mean(x), 1))

  # test 4
  set.seed(1)
  x <- DMCfun::randBeta(n = 10000, lim = c(-10, 10))
  testthat::expect_equal(length(x), 10000)
  testthat::expect_equal(0, round(mean(x), 1))

  # test 5
  set.seed(1)
  x <- DMCfun::randBeta(n = 10000, lim = c(0, 10))
  testthat::expect_equal(length(x), 10000)
  testthat::expect_equal(5, round(mean(x), 1))

})
