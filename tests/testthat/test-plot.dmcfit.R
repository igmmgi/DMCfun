context("plot.dmcfit")

test_that("plot.dmcfit", {
  
  # just check code does not error
  # test 1
  resTh <- dmcFit(DMCfun::flankerData, nTrl = 1000,
                  printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData), NA)
  
  # test 2
  resTh <- dmcFit(DMCfun::simonData, nTrl = 1000,
                  printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::simonData), NA)
  
  # test 3
  resTh <- dmcFitSubject(DMCfun::flankerData, nTrl = 1000, subjects = 10,
                         printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, DMCfun::flankerData, subject = 10), NA)
  
  # test 4
  dat <- createDF(nSubjects = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp_comp"   = c(500, 150, 100),
                             "Comp_incomp" = c(530, 150, 150)),
                   Error = list("Comp_comp"   = c(5, 3, 2, 1, 1),
                                "Comp_incomp" = c(15, 12, 5, 2, 1)))
  datOb <- dmcObservedData(dat)
  
  resTh <- dmcFit(datOb, nTrl = 1000, printInputArgs = FALSE, printResults = FALSE)
  testthat::expect_error(plot(resTh, datOb), NA)
  
})
