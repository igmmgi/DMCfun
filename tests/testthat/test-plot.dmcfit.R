context("plot.dmcfit")

test_that("plot.dmcfit", {

  # just check code does not error
  # test 1
  resTh <- dmcFitAgg(DMCfun::flankerData, nTrl = 1000,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData), NA)

  # test 2
  resTh <- dmcFitAgg(DMCfun::simonData, nTrl = 1000,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::simonData), NA)

  # test 3
  resTh <- dmcFitVPs(DMCfun::flankerData, nTrl = 1000, VP = 10,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData, VP = 10), NA)

  # test 4
  dat <- createDF(nVP = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp_comp"   = c(500, 150, 100),
                             "Comp_incomp" = c(530, 150, 150)),
                   Error = list("Comp_comp"   = c(5, 3, 2, 1, 1),
                                "Comp_incomp" = c(15, 12, 5, 2, 1)))
  datOb <- dmcObservedData(dat)

  resTh <- dmcFitAgg(datOb, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, datOb), NA)

})
