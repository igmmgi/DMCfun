context("dmcSim")

test_that("dmcSim1", {

  # Simulation 1 (Figure 3)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 30, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(440, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(105, round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.6, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(459, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(94,  round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(1.3, round(dat$summary$perErr[2], 1)) # percentage errors

})

test_that("dmcSim2", {

  # Simulation 2 (Figure 4)
  # amp = 20, tau = 150, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 150, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(421, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(90,  round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.2, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(484, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(102, round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(2.2, round(dat$summary$perErr[2], 1)) # percentage errors

})

test_that("dmcSim3", {

  # Simulation 3 (Figure 5)
  # amp = 20, tau = 90, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 90, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(421, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(96,  round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.3, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(479, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(96,  round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(2.2, round(dat$summary$perErr[2], 1)) # percentage errors

})

test_that("dmcSim4", {

  # Simulation 3 (Figure 6)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(spDist = 1, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(436, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(117, round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(1.9, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(452, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(100, round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(6.9, round(dat$summary$perErr[2], 1)) # percentage errors

})

test_that("dmcSim5", {

  # Simulation 3 (Figure 7)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(drDist = 1, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(477, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(146, round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(3.1, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(495, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(134, round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(4.1, round(dat$summary$perErr[2], 1)) # percentage errors

  # Values from paper?
  # testthat::expect_equal(500,  round(dat$means$rtCor[1]))       # rt correct
  # testthat::expect_equal(175,  round(dat$means$sdRtCor[1]))     # sd correct
  # testthat::expect_equal(12.1, round(dat$means$perErr[1], 1))   # percentage errors
  # testthat::expect_equal(522,  round(dat$means$rtCor[2]))       # rt correct
  # testthat::expect_equal(164,  round(dat$means$sdRtCor[2]))     # sd correct
  # testthat::expect_equal(13.9, round(dat$means$perErr[2], 1))   # percentage errors

})

test_that("dmcSim6", {

  amp <- 20
  drc <- 0.5

  dmc <- dmcSim(amp = amp, drc = drc)

  testthat::expect_equal(dmc$prms$amp, 20)
  testthat::expect_equal(dmc$prms$drc, 0.5)

})

test_that("dmcSim7", {

  dmc <- dmcSim(pDelta = c(10, 30, 50, 70, 90))
  testthat::expect_equal(nrow(dmc$delta), 5)

  dmc <- dmcSim(pDelta = seq(10, 90, 10), tDelta = 2)
  testthat::expect_equal(nrow(dmc$delta), 10)

})

test_that("dmcSim8", {

  params <- list(amp = seq(10, 20, 10))
  dmc    <- dmcSims(params)
  testthat::expect_equal(length(dmc), 2)

  params <- list(amp = seq(10, 20, 5), tau = c(50, 100, 150), nTrl = 10000)
  dmc    <- dmcSims(params)
  testthat::expect_equal(length(dmc), 9)

})
