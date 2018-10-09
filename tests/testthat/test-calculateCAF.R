context("calculateCAF")

test_that("calculateCAF", {

  # test 1: 10% errors, 5 bins
  set.seed(1)
  rts  <- rtDist(1000)
  errs <- errDist(1000, 10)
  dat  <- cbind(rts, errs)
  caf  <- calculateCAF(dat)

  testthat::expect_equal(length(caf), 5)
  testthat::expect_equal(0.9, round(mean(caf), 1))

  # test 2 10% errors, 10 bins
  set.seed(1)
  rts  <- rtDist(1000)
  errs <- errDist(1000, 10)
  dat  <- cbind(rts, errs)
  caf  <- calculateCAF(dat, stepCAF = 10)

  testthat::expect_equal(length(caf), 10)
  testthat::expect_equal(0.9, round(mean(caf), 1))

  # test 3: 20% errors, 5 bins
  set.seed(1)
  rts  <- rtDist(1000)
  errs <- errDist(1000, 20)
  dat  <- cbind(rts, errs)
  caf  <- calculateCAF(dat)

  testthat::expect_equal(length(caf), 5)
  testthat::expect_equal(0.8, round(mean(caf), 1))

  # test 4: 30% errors, 8 bins
  set.seed(1)
  rts  <- rtDist(1000)
  errs <- errDist(1000, 30)
  dat  <- cbind(rts, errs)
  caf  <- calculateCAF(dat, stepCAF = 25)

  testthat::expect_equal(length(caf), 4)
  testthat::expect_equal(0.7, round(mean(caf), 1))

})
