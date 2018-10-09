context("errDist")

test_that("errDist", {

  # test 1: 10% errors, 5 bins
  set.seed(1)
  rts  <- rtDist(1000)
  errs <- errDist(1000, 10)
  dat  <- cbind(rts, errs)
  res  <- calculateCAF(dat)

  testthat::expect_equal(length(res), 5)
  testthat::expect_equal(0.9, round(mean(res), 1))

  # test 2: 20% errors, 5 bins
  set.seed(1)
  rts  <- rtDist(1000)
  errs <- errDist(1000, 20)
  dat  <- cbind(rts, errs)
  res  <- calculateCAF(dat)

  testthat::expect_equal(length(res), 5)
  testthat::expect_equal(0.8, round(mean(res), 1))

  # test 3: 30% errors, 10 bins
  set.seed(1)
  rts  <- rtDist(1000)
  errs <- errDist(1000, 30)
  dat  <- cbind(rts, errs)
  res  <- calculateCAF(dat, stepCAF = 10)

  testthat::expect_equal(length(res), 10)
  testthat::expect_equal(0.7, round(mean(res), 1))

  # test 5: 5% errors, 2 bins
  set.seed(1)
  rts  <- rtDist(10000)
  errs <- errDist(10000, 5)
  dat  <- cbind(rts, errs)
  res  <- calculateCAF(dat, stepCAF = 50)

  testthat::expect_equal(length(res), 2)
  testthat::expect_equal(0.95, round(mean(res), 2))

})
