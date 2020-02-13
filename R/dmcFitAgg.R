#' @title dmcFitAgg
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions.
#'
#' @param resOb Observed data (see flankerData1, flankerData2,
#' simonTask1 for data format)
#' @param nTrl Number of trials to use within dmcSim
#' @param startVals Starting values for to-be estimated parameters
#' @param minVals Minimum values for the to-be estimated parameters
#' @param maxVals Maximum values for the to-be estimated parameters
#' @param fixed Fix parameter to starting value
#' @param parScale Scaling values for the to-be estimated parameters
#' @param fitInitialTau TRUE/FALSE
#' @param fitInitialGrid TRUE/FALSE
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would result
#' in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#' @param stepDelta Step size for the Delta bins. For example, a step size of 5 would result
#' in 19 CAF bins positioned at 5, 10, 15, ... 85, 90, 95\%.
#' @param printInputArgs TRUE/FALSE
#' @param printResults TRUE/FALSE
#'
#' @return resTh
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1: Flanker data from Ulrich et al. (2015)
#' fit <- dmcFitAgg(flankerData1, fitInitialTau = TRUE, fitInitialGrid = TRUE)
#' plot(fit, flankerData1)
#' summary(fit)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFitAgg(simonData1)
#' plot(fit, simonData1)
#' summary(fit)
#'
#' # Example 3: Flanker Example Data
#' fit <- dmcFitAgg(flankerData2)
#' plot(fit, flankerData1)
#' summary(fit)
#'
#' # Example 4: Simon Example Data
#' fit <- dmcFitAgg(simonData2)
#' plot(fit, simonData1)
#' summary(fit)
#'
#' # Example 5: Simulated Data
#' dat <- createDF(nVP = 50, nTrl = 150,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp"), vals = c(510, 100, 100)),
#'                            list(c("Comp:incomp"), vals = c(540, 120, 150))),
#'                  Error = list(list(c("Comp:comp"), vals = c(3, 2, 2, 1, 1)),
#'                             list(c("Comp:incomp"), vals = c(20, 3, 2, 1, 1))))
#' datOb <- dmcObservedData(dat, columns = c("vp", "comp", "rt", "error"))
#' plot(datOb)
#' fit <- dmcFitAgg(datOb)
#' plot(fit, datOb)
#' summary(fit)
#'
#' }
#'
#' @export
dmcFitAgg <- function(resOb,
                      nTrl           = 50000,
                      startVals      = c(20, 100, 0.5,  75, 300,  30, 2, 3),
                      minVals        = c(10,   5, 0.1,  20, 200,   5, 1, 2),
                      maxVals        = c(30, 300, 1.0, 150, 800, 100, 3, 4),
                      fixed          = c( 0,   0, 0,     0,   0,   0, 0, 0),
                      parScale       = startVals/min(startVals),
                      fitInitialTau  = TRUE,
                      fitInitialGrid  = FALSE,
                      stepCAF        = 20,
                      stepDelta      = 5,
                      printInputArgs = TRUE,
                      printResults   = FALSE) {

  prms <- list("amp" = startVals[1],
               "tau" = startVals[2],
               "mu" = startVals[3],
               "bnds" = startVals[4],
               "resMean" = startVals[5],
               "resSD" = startVals[6],
               "aaShape" = startVals[7],
               "spShape" = startVals[8])

  # check observed data contains correct number of delta/CAF bins
  if (nrow(resOb$delta) != length(seq(stepDelta, 100 - stepDelta, stepDelta))) {
    stop("Number of delta bins in observed data and nDelta bins are not equal!")
  }
  if ((nrow(resOb$caf)/2) != length(seq(0, 100 - stepCAF, stepCAF))) {
    stop("Number of CAF bins in observed data and nCAF bins are not equal!")
  }

  # function to minimise
  minimizeCostValue <- function(x, prms, fixed, resOb, nTrl, stepDelta, stepCAF, printInputArgs, printResults) {

    prms[!as.logical(fixed)] <- x

    resTh <- dmcSim(amp = prms$amp, tau = prms$tau, mu = prms$mu,
                    bnds = prms$bnds, resMean = prms$resMean, resSD = prms$resSD,
                    aaShape = prms$aaShape,
                    varSP = TRUE, spShape = prms$spShape, spLim = c(-prms$bnds, prms$bnds),
                    nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                    printInputArgs = printInputArgs, printResults = printResults)

    return(calculateCostValue(resTh, resOb))

  }

  ############################# FIT PROCEDURE ##################################
  if (fitInitialTau) {

    cl <- parallel::makeCluster(parallel::detectCores())
    doSNOW::registerDoSNOW(cl)

    start_vals_tau <- seq(minVals[2], maxVals[2], length.out = 10)
    message("Searching initial tau gridspace: N = ", length(start_vals_tau))

    # progress bar
    pb <- txtProgressBar(min = 0, max = length(start_vals_tau), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)

    costValue <- foreach::foreach(i = start_vals_tau, .packages = "DMCfun", .options.snow = list(progress = progress)) %dopar% {

      resTh <- dmcSim(amp = startVals[1], tau = i, mu = startVals[3],
                      bnds = startVals[4], resMean = startVals[5], resSD = startVals[6],
                      aaShape = startVals[7],
                      varSP = TRUE, spShape = startVals[8], spLim = c(-startVals[4], startVals[4]),
                      nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                      printInputArgs = printInputArgs, printResults = FALSE)
      return(calculateCostValue(resTh, resOb))
    }

    # adjust start, min and max vals
    startVals[2] <- as.numeric(start_vals_tau[which.min(costValue)])
    minVals[2]   <- startVals[2] * 0.8
    maxVals[2]   <- startVals[2] * 1.2

    close(pb)
    stopCluster(cl)

  }

  ############################# FIT PROCEDURE ##################################
  if (fitInitialGrid) {

    cl <- makeCluster(parallel::detectCores())
    doSNOW::registerDoSNOW(cl)

    start_vals <- expand.grid(Map(seq, minVals, maxVals, length.out = 5))
    message("Searching initial parameter gridspace: N = ", nrow(start_vals))

    # progress bar
    pb <- txtProgressBar(min = 0, max = nrow(start_vals), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)

    costValue <- foreach:.foreach(i = 1:nrow(start_vals), .packages = "DMCfun", .options.snow = list(progress = progress)) %dopar% {

      resTh <- dmcSim(amp = start_vals[i, 1], tau = start_vals[i, 2], mu = start_vals[i, 3],
                      bnds = start_vals[i, 4], resMean = start_vals[i, 5], resSD = start_vals[i, 6],
                      aaShape = start_vals[i, 7],
                      varSP = TRUE, spShape = start_vals[i, 8], spLim = c(-start_vals[i, 4], start_vals[i, 4]),
                      nTrl = 10000, stepDelta = stepDelta, stepCAF = stepCAF,
                      printInputArgs = printInputArgs, printResults = FALSE)
      return(calculateCostValue(resTh, resOb))

    }

    # adjust start, min and max vals
    startVals <- as.numeric(start_vals[which.min(costValue), ])
    minVals <- startVals * 0.8
    maxVals <- startVals * 1.2

    close(pb)
    stopCluster(cl)

  }

  # optimize
  fit <- optimr::optimr(par = startVals[!as.logical(fixed)], fn = minimizeCostValue,
                        prms = prms, fixed = fixed, resOb = resOb,
                        nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                        printInputArgs = printInputArgs, printResults = printResults,
                        method = "L-BFGS-B", lower = minVals[!as.logical(fixed)], upper = maxVals[!as.logical(fixed)],
                        control = list(parscale = parScale[!as.logical(fixed)]))

  prms[!as.logical(fixed)] <- fit$par
  fit$par <- as.vector(unlist(prms))

  resTh <- dmcSim(amp = prms$amp, tau = prms$tau, mu = prms$mu,
                  bnds = prms$bnds, resMean = prms$resMean, resSD = prms$resSD,
                  aaShape = prms$aaShape,
                  varSP = TRUE, spShape = prms$spShape, spLim = c(-prms$bnds, prms$bnds),
                  nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                  printResults = FALSE)

  cat(sprintf("RMSE: %.3f\n", fit$value))

  dmcfit <- list(resTh, fit)
  class(dmcfit) <- c("dmcfit")

  return(dmcfit)

}
