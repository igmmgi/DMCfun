context("dmcData")

test_that("createDF", {

  # default
  dat <- createDF()
  testthat::expect_equal(nrow(dat), 4000)
  testthat::expect_equal(ncol(dat), 3)
  testthat::expect_equal(names(dat), c("Subject", "A", "B"))

  # 1 factor, 2 levels
  dat <- createDF(nSubjects = 20, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  testthat::expect_equal(nrow(dat), 20 * 50 * 2)
  testthat::expect_equal(ncol(dat), 2)
  testthat::expect_equal(names(dat), c("Subject", "Comp"))

  # 1 factor, 3 levels
  dat <- createDF(nSubjects = 15, nTrl = 25,
                  design = list("Comp" = c("neutral", "comp", "incomp")))
  testthat::expect_equal(nrow(dat), 15 * 25 * 3)
  testthat::expect_equal(ncol(dat), 2)
  testthat::expect_equal(names(dat), c("Subject", "Comp"))

  # 2*2 factor, 2 levels each
  dat <- createDF(nSubjects = 10, nTrl = 25,
                  design = list("Comp" = c("comp", "incomp"),
                                "Side" = c("left", "right")))
  testthat::expect_equal(nrow(dat), 10 * 25 * 2 * 2)
  testthat::expect_equal(ncol(dat), 3)
  testthat::expect_equal(names(dat), c("Subject", "Comp", "Side"))

})

test_that("addDataDF", {

  # default 2*2
  dat <- createDF(nTrl = 100)
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 8000)
  testthat::expect_equal(ncol(dat), 5)
  testthat::expect_equal(names(dat), c("Subject", "A", "B", "RT", "Error"))

  # 1 factor with 2 levels
  dat <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 20 * 50 * 2)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("Subject", "Comp", "RT", "Error"))

  # 1 factor with 3 levels
  dat <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 20 * 50 * 3)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("Subject", "Comp", "RT", "Error"))

  # 2 factors, 3 & 2 levels
  dat <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral"),
                                                           "Side" = c("left", "right")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 20 * 50 * 6)
  testthat::expect_equal(ncol(dat), 5)
  testthat::expect_equal(names(dat), c("Subject", "Comp", "Side", "RT", "Error"))

  # 1 factor, 1 VP, 1 trial
  dat <- createDF(nSubjects = 1, nTrl = 1, design = list("A" = c("a", "b")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 2)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("Subject", "A", "RT", "Error"))

  # Further tests with RT & Error parameters defined
  # 1 factor with 2 levels, 1 VP, 1 trial
  dat <- createDF(nSubjects = 1, nTrl = 1, design = list("A" = c("a", "b")))
  dat <- addDataDF(dat,  RT = c(500, 100, 100),  Error = 10)

  testthat::expect_equal(nrow(dat), 2)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("Subject", "A", "RT", "Error"))

  # 1 factor with 2 levels, 2 VPs, 2 trials
  dat <- createDF(nSubjects = 2, nTrl = 2, design = list("A" = c("a", "b")))
  dat <- addDataDF(dat,  RT = c(500, 100, 100),  Error = 10)

  testthat::expect_equal(nrow(dat), 8)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("Subject", "A", "RT", "Error"))

  # Further tests with RT & Error parameters defined
  # 1 factor, 1 VP, 1 trial
  dat <- createDF(nSubjects = 1, nTrl = 1, design = list("A" = c("a", "b")))
  dat <- addDataDF(dat,
                   RT = list("A_a" = c(500, 100, 100),
                             "A_b" = c(600, 100, 100)),
                   Error = list("A_a" = 10,
                                "A_b" = 20))

  testthat::expect_equal(nrow(dat), 2)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("Subject", "A", "RT", "Error"))

  # 1 factor with 2 levels, 2 VPs, 4 trials
  dat <- createDF(nSubjects = 2, nTrl = 4, design = list("A" = c("a", "b")))
  dat <- addDataDF(dat,
                   RT = list("A_a" = c(500, 100, 100),
                             "A_b" = c(600, 100, 100)),
                   Error = list("A_a" = 10,
                                "A_b" = 20))

  testthat::expect_equal(nrow(dat), 16)
  testthat::expect_equal(ncol(dat), 4)



})

test_that("calculateCAF", {

  set.seed(1)

  # test 1: 10% errors, 5 bins
  dat  <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat  <- addDataDF(dat)
  caf  <- calculateCAF(dat)

  testthat::expect_equal(length(unique(caf$Bin)), 5)
  testthat::expect_equal(round(mean(caf$comp), 1),  0.9)

  # test 2 10% errors, 10 bins
  dat  <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat  <- addDataDF(dat)
  caf  <- calculateCAF(dat, nCAF = 10)

  testthat::expect_equal(length(unique(caf$Bin)), 10)
  testthat::expect_equal(round(mean(caf$incomp), 1),  0.9)

  # test 3: 20% errors, 5 bins
  dat  <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat  <- addDataDF(dat, Error = 20)
  caf  <- calculateCAF(dat, nCAF = 5)

  testthat::expect_equal(length(unique(caf$Bin)), 5)
  testthat::expect_equal(round(mean(caf$comp), 1),  0.8)

  # test 4: 30% errors, 4 bins
  dat  <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat  <- addDataDF(dat, Error = 30)
  caf  <- calculateCAF(dat, nCAF = 4)

  testthat::expect_equal(length(unique(caf$Bin)), 4)
  testthat::expect_equal(round(mean(caf$comp), 1),  0.7)

})

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

test_that("errDist", {

  set.seed(1)

  # test 1: 10% errors
  errs <- DMCfun:::errDist(10000, 10)

  testthat::expect_equal(length(errs), 10000)
  testthat::expect_equal(0.1, round(mean(errs), 1))

  # test 2: 20% errors
  errs <- DMCfun:::errDist(10000, 20)

  testthat::expect_equal(length(errs), 10000)
  testthat::expect_equal(0.2, round(mean(errs), 1))

})

test_that("dmcObservedData", {

  # create data frame
  dat <- createDF(nSubjects = 50, nTrl = 100,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat)
  datOb <- dmcObservedData(dat)

  testthat::expect_type(datOb, "list")
  testthat::expect_s3_class(datOb, "dmcob")

  dat <- createDF(nSubjects = 50, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp_comp"   = c(500, 150, 100),
                             "Comp_incomp" = c(530, 150, 150)),
                   Error = list("Comp_comp"   = c(5, 2, 2, 1, 1),
                                "Comp_incomp" = c(7, 4, 2, 1, 1)))
  datOb <- dmcObservedData(dat, tDelta = 2)

  testthat::expect_type(datOb, "list")
  testthat::expect_s3_class(datOb, "dmcob")

})



