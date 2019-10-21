context("plot.dmcfit")

test_that("plot.dmcfit", {

  # just check code does not error
  # test 1
  resTh <- dmcFitAgg(DMCfun::flankerData1, nTrl = 1000,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData1), NA)

  # test 2
  resTh <- dmcFitAgg(DMCfun::flankerData2, nTrl = 1000,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData2), NA)

  # test 3
  resTh <- dmcFitAgg(DMCfun::simonData1, nTrl = 1000,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::simonData1), NA)

  # test 4
  resTh <- dmcFitVPs(DMCfun::flankerData1, nTrl = 1000, VP = 10,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData1, VP = 10), NA)

  # test 5
  resTh <- dmcFitVPs(DMCfun::simonData1, nTrl = 1000, VP = 4,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData1, VP = 4), NA)

  # test 6
  dat <- createDF(nVP = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list(list(c("Comp:comp"), vals = c(500, 150, 100)),
                             list(c("Comp:incomp"), vals = c(530, 150, 150))),
                   Error = list(list(c("Comp:comp"), vals = c(5, 3, 2, 1, 1)),
                              list(c("Comp:incomp"), vals = c(15, 12, 5, 2, 1))))
  datOb <- dmcObservedData(dat)

  resTh <- dmcFitAgg(datOb, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, datOb), NA)

  # test 7
  dat <- createDF(nVP = 2, nTrl = 5000,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list(list(c("Comp:comp"), vals = c(500, 30, 100)),
                             list(c("Comp:incomp"), vals = c(530, 30, 130))),
                   Error = list(list(c("Comp:comp"), vals = c(5, 3, 2, 1, 1)),
                              list(c("Comp:incomp"), vals = c(15, 12, 5, 2, 1))))
  datOb <- dmcObservedData(dat)

  resTh <- dmcFitVPs(datOb, nTrl = 5000, VP = 2,
                     printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, datOb, VP = 2), NA)

})
