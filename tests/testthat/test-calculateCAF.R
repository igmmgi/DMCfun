context("calculateCAF")

test_that("calculateCAF", {

  set.seed(1)
  
  # test 1: 10% errors, 5 bins
  dat  <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat  <- addDataDF(dat)
  caf  <- calculateCAF(dat)

  testthat::expect_equal(length(unique(caf$bin)), 5)
  testthat::expect_equal(round(mean(caf$accPer), 1),  0.9)

  # test 2 10% errors, 10 bins
  dat  <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat  <- addDataDF(dat)
  caf  <- calculateCAF(dat, nCAF = 10)

  testthat::expect_equal(length(unique(caf$bin)), 10)
  testthat::expect_equal(round(mean(caf$accPer), 1),  0.9)

  # test 3: 20% errors, 5 bins
  dat  <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat  <- addDataDF(dat, Error = 20)
  caf  <- calculateCAF(dat, nCAF = 5)

  testthat::expect_equal(length(unique(caf$bin)), 5)
  testthat::expect_equal(round(mean(caf$accPer), 1),  0.8)

  # test 4: 30% errors, 4 bins
  dat  <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat  <- addDataDF(dat, Error = 30)
  caf  <- calculateCAF(dat, nCAF = 4)

  testthat::expect_equal(length(unique(caf$bin)), 4)
  testthat::expect_equal(round(mean(caf$accPer), 1),  0.7)


})
