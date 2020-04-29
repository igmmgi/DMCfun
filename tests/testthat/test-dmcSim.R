context("dmcSim")

test_that("dmcSim1", {

  # Simulation 1 (Figure 3)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 30, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(440, round(dat$means$rtCor[1]))     # rt correct
  testthat::expect_equal(105, round(dat$means$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.6, round(dat$means$perErr[1], 1)) # percentage errors
  testthat::expect_equal(459, round(dat$means$rtCor[2]))     # rt correct
  testthat::expect_equal(94,  round(dat$means$sdRtCor[2]))   # sd correct
  testthat::expect_equal(1.3, round(dat$means$perErr[2], 1)) # percentage errors

})

test_that("dmcSim2", {

  # Simulation 2 (Figure 4)
  # amp = 20, tau = 150, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 150, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(421, round(dat$means$rtCor[1]))     # rt correct
  testthat::expect_equal(90,  round(dat$means$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.3, round(dat$means$perErr[1], 1)) # percentage errors
  testthat::expect_equal(484, round(dat$means$rtCor[2]))     # rt correct
  testthat::expect_equal(103, round(dat$means$sdRtCor[2]))   # sd correct
  testthat::expect_equal(2.2, round(dat$means$perErr[2], 1)) # percentage errors

})

test_that("dmcSim3", {

  # Simulation 3 (Figure 5)
  # amp = 20, tau = 90, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 90, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(421, round(dat$means$rtCor[1]))     # rt correct
  testthat::expect_equal(96,  round(dat$means$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.3, round(dat$means$perErr[1], 1)) # percentage errors
  testthat::expect_equal(479, round(dat$means$rtCor[2]))     # rt correct
  testthat::expect_equal(96,  round(dat$means$sdRtCor[2]))   # sd correct
  testthat::expect_equal(2.2, round(dat$means$perErr[2], 1)) # percentage errors

})

test_that("dmcSim4", {

  # Simulation 3 (Figure 6)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(varSP = TRUE, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(435, round(dat$means$rtCor[1]))     # rt correct
  testthat::expect_equal(117, round(dat$means$sdRtCor[1]))   # sd correct
  testthat::expect_equal(1.9, round(dat$means$perErr[1], 1)) # percentage errors
  testthat::expect_equal(452, round(dat$means$rtCor[2]))     # rt correct
  testthat::expect_equal(100, round(dat$means$sdRtCor[2]))   # sd correct
  testthat::expect_equal(6.8, round(dat$means$perErr[2], 1)) # percentage errors

})

test_that("dmcSim5", {

  # Simulation 3 (Figure 7)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(varDR = TRUE, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(477, round(dat$means$rtCor[1]))     # rt correct
  testthat::expect_equal(145, round(dat$means$sdRtCor[1]))   # sd correct
  testthat::expect_equal(3.1, round(dat$means$perErr[1], 1)) # percentage errors
  testthat::expect_equal(494, round(dat$means$rtCor[2]))     # rt correct
  testthat::expect_equal(134, round(dat$means$sdRtCor[2]))   # sd correct
  testthat::expect_equal(4.1, round(dat$means$perErr[2], 1)) # percentage errors

  # Values from paper?
  # testthat::expect_equal(500,  round(dat$means$rtCor[1]))       # rt correct
  # testthat::expect_equal(175,  round(dat$means$sdRtCor[1]))     # sd correct
  # testthat::expect_equal(12.1, round(dat$means$perErr[1], 1))   # percentage errors
  # testthat::expect_equal(522,  round(dat$means$rtCor[2]))       # rt correct
  # testthat::expect_equal(164,  round(dat$means$sdRtCor[2]))     # sd correct
  # testthat::expect_equal(13.9, round(dat$means$perErr[2], 1))   # percentage errors

})

test_that("dmcSim6", {

  amp = 20
  mu = 0.5
  
  dmc <- dmcSim(amp = amp, mu = mu)

  testthat::expect_equal(dmc$prms$amp, 20)
  testthat::expect_equal(dmc$prms$mu, 0.5)

})

