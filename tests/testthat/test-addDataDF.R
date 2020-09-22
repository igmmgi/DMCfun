context("addDataDF")

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

  testthat::expect_equal(nrow(dat), 20*50*2)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("Subject", "Comp", "RT", "Error"))

  # 1 factor with 3 levels
  dat <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 20*50*3)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("Subject", "Comp", "RT", "Error"))

  # 2 factors, 3 & 2 levels
  dat <- createDF(nSubjects = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral"),
                                                           "Side" = c("left", "right")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 20*50*6)
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
