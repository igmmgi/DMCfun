context("dmcSim")

test_that("dmcSim1", {

  # Simulation 1 (Figure 3)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 30, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  # check that RTs/SDs within 1.5 ms and error rate within 0.5%
  testthat::expect_equal(440, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(105, round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.7, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(458, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(95,  round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(1.3, round(dat$summary$perErr[2], 1)) # percentage errors

})

# test_that("dmcSim1_base", {
#
#   # Simulation 1 (Figure 3)
#   # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
#   dat <- dmcSimR(tau = 30, setSeed = TRUE)
#
#   # check that RTs/SDs within 1.5 ms and error rate within 0.5%
#   testthat::expect_equal(440, round(dat$summary$rtCor[1]))     # rt correct
#   testthat::expect_equal(105, round(dat$summary$sdRtCor[1]))   # sd correct
#   testthat::expect_equal(0.7, round(dat$summary$perErr[1], 1)) # percentage errors
#   testthat::expect_equal(459, round(dat$summary$rtCor[2]))     # rt correct
#   testthat::expect_equal(94,  round(dat$summary$sdRtCor[2]))   # sd correct
#   testthat::expect_equal(1.3, round(dat$summary$perErr[2], 1)) # percentage errors
#
# })

test_that("dmcSim2", {

  # Simulation 2 (Figure 4)
  # amp = 20, tau = 150, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 150, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(421, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(90,  round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.3, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(484, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(103, round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(2.3, round(dat$summary$perErr[2], 1)) # percentage errors

})

test_that("dmcSim3", {

  # Simulation 3 (Figure 5)
  # amp = 20, tau = 90, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(tau = 90, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(421, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(96,  round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(0.3, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(478, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(96,  round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(2.3, round(dat$summary$perErr[2], 1)) # percentage errors

})

test_that("dmcSim4", {

  # Simulation 3 (Figure 6)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(varSP = TRUE, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(435, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(117, round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(1.8, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(451, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(100, round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(6.9, round(dat$summary$perErr[2], 1)) # percentage errors

})

test_that("dmcSim5", {

  # Simulation 3 (Figure 7)
  # amp = 20, tau = 30, mu = 0.5, sigm = 4, bnds = 75, resMean = 300, resSD = 30
  dat <- dmcSim(varDR = TRUE, printInputArgs = FALSE, printResults = FALSE, setSeed = TRUE)

  testthat::expect_equal(477, round(dat$summary$rtCor[1]))     # rt correct
  testthat::expect_equal(146, round(dat$summary$sdRtCor[1]))   # sd correct
  testthat::expect_equal(3.1, round(dat$summary$perErr[1], 1)) # percentage errors
  testthat::expect_equal(494, round(dat$summary$rtCor[2]))     # rt correct
  testthat::expect_equal(134, round(dat$summary$sdRtCor[2]))   # sd correct
  testthat::expect_equal(4.2, round(dat$summary$perErr[2], 1)) # percentage errors

  # Values from paper?
  # testthat::expect_equal(500, round(dat$summary$rtCor[1]))       # rt correct
  # testthat::expect_equal(175,  round(dat$summary$sdRtCor[1]))    # sd correct
  # testthat::expect_equal(12.1, round(dat$summary$perErr[1], 1))  # percentage errors
  # testthat::expect_equal(522,  round(dat$summary$rtCor[2]))      # rt correct
  # testthat::expect_equal(164,  round(dat$summary$sdRtCor[2]))    # sd correct
  # testthat::expect_equal(13.9,  round(dat$summary$perErr[2], 1)) # percentage errors

})
