context("calculateCostValue")

test_that("calculateCostValue", {

  set.seed(1)

  # test 1:
  resTh <- dmcSim()
  resOb <- dmcSim()
  cost  <- calculateCostValueRMSE(resTh, resOb)

  testthat::expect_lt(cost, 3)

  # test 2:
  resTh <- dmcSim()
  resOb <- dmcSim(tau = 150)
  cost  <- calculateCostValueRMSE(resTh, resOb)

  testthat::expect_gt(cost, 20)

})
