context("dmcFit")

test_that("dmcFit", {

  # dmcFit
  fit <- dmcFit(DMCfun::flankerData, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE, optimControl = list(maxit = 20))
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")

  fit <- dmcFit(DMCfun::simonData, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE, fitInitialGrid = FALSE, optimControl = list(maxit = 20))
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")

  fit <- dmcFit(DMCfun::flankerData,  nTrl = 1000, printInputArgs = FALSE, printResults = FALSE,
    costFunction = "SPE", startVals = list(drc = 0.6, aaShape = 2.5),  fixedFit = list(drc = T, aaShape = T), optimControl = list(maxit = 20))
  testthat::expect_equal(fit$par$drc, 0.6)
  testthat::expect_equal(fit$par$aaShape, 2.5)

  testthat::expect_error(dmcFit(DMCfun::flankerData, nDelta = 99))
  testthat::expect_error(dmcFit(DMCfun::flankerData, nCAF = 99))

  # dmcFitSubject
  fit <- dmcFitSubject(DMCfun::flankerData, nTrl = 1000, subjects = c(1, 2),
    printInputArgs = FALSE, printResults = FALSE, optimControl = list(maxit = 20))
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit_subject")
  testthat::expect_type(mean(fit), "list")

  fit <- dmcFitSubject(DMCfun::flankerData,  nTrl = 1000, subjects = c(1, 2),
    printInputArgs = FALSE, printResults = FALSE,
    costFunction = "SPE", startVals = list(drc = 0.6, aaShape = 2.5),  fixedFit = list(drc = T, aaShape = T), optimControl = list(maxit = 20))
  testthat::expect_equal(fit[[1]]$par$drc, 0.6)
  testthat::expect_equal(fit[[1]]$par$aaShape, 2.5)

  testthat::expect_error(dmcFitSubject(DMCfun::flankerData, nDelta = 99, subjects = c(1, 2)))
  testthat::expect_error(dmcFitSubject(DMCfun::flankerData, nCAF = 99, subjects = c(1, 2)))

  # dmcFitDE
  fit <- dmcFitDE(DMCfun::flankerData, nTrl = 1000, deControl = list(itermax = 20))
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")

  fit <- dmcFitDE(DMCfun::simonData, nTrl = 1000, costFunction = "SPE", deControl = list(itermax = 20))
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit")

  testthat::expect_error(dmcFitDE(DMCfun::flankerData, nDelta = 99))
  testthat::expect_error(dmcFitDE(DMCfun::flankerData, nCAF = 99))

  # dmcFitSubjectDE
  fit <- dmcFitSubjectDE(DMCfun::flankerData, nTrl = 1000, subjects = c(1, 2), deControl = list(itermax = 20))
  testthat::expect_type(fit, "list")
  testthat::expect_s3_class(fit, "dmcfit_subject")

  # should error
  testthat::expect_error(dmcFitSubjectDE(DMCfun::flankerData, nDelta = 99))
  testthat::expect_error(dmcFitSubjectDE(DMCfun::flankerData, nCAF = 99))

})
