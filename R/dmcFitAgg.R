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
#' @param fixedFit Fix parameter to starting value
#' @param parScale Scaling values for the to-be estimated parameters
#' @param fitInitialGrid TRUE/FALSE (NB. overrides fitInitialTau)
#' @param fitInitialGridN 10
#' @param fixedGrid Fix parameter for initial grid search
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
#' fit <- dmcFitAgg(flankerData1)
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
#' # Example 5: Simulated Data (+ve going delta function)
#' dat <- createDF(nVP = 20, nTrl = 500,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp"), vals = c(510, 100, 100)),
#'                            list(c("Comp:incomp"), vals = c(540, 130, 85))),
#'                  Error = list(list(c("Comp:comp"), vals = c(4, 3, 2, 1, 1)),
#'                             list(c("Comp:incomp"), vals = c(20, 4, 3, 1, 1))))
#' datOb <- dmcObservedData(dat, columns = c("VP", "Comp", "RT", "Error"),
#'                          stepCAF = 20, stepDelta = 10)
#' plot(datOb)
#' fit <- dmcFitAgg(datOb, stepCAF = 20, stepDelta = 10, fitInitialGrid = TRUE)
#' plot(fit, datOb)
#' summary(fit)
#' }
#'
#' @export
dmcFitAgg <- function(resOb,
                      nTrl             = 50000,
                      startVals        = c(20, 100, 0.5,  75, 300,  30, 2, 3),
                      minVals          = c(10,   5, 0.1,  20, 200,   5, 1, 2),
                      maxVals          = c(30, 300, 1.0, 150, 800, 100, 3, 4),
                      fixedFit         = c( 0,   0, 0,     0,   0,   0, 0, 0),
                      parScale         = startVals/min(startVals),
                      fitInitialGrid   = TRUE,
                      fitInitialGridN  = 10,                                    # reduce if grid search 3/4+ parameters
                      fixedGrid        = c( 1,   0, 1,     1,   1,   1, 1, 1),  # only fit tau
                      stepCAF          = 20,
                      stepDelta        = 5,
                      printInputArgs   = TRUE,
                      printResults     = FALSE) {

  prms <- list("amp"     = startVals[1],
               "tau"     = startVals[2],
               "mu"      = startVals[3],
               "bnds"    = startVals[4],
               "resMean" = startVals[5],
               "resSD"   = startVals[6],
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
  minimizeCostValue <- function(x, prms, fixedFit, resOb, nTrl, stepDelta, stepCAF, printInputArgs, printResults) {

    prms[!as.logical(fixedFit)] <- x

    resTh <- dmcSim(amp = prms$amp, tau = prms$tau, mu = prms$mu,
                    bnds = prms$bnds, resMean = prms$resMean, resSD = prms$resSD,
                    aaShape = prms$aaShape,
                    varSP = TRUE, spShape = prms$spShape, spLim = c(-prms$bnds, prms$bnds),
                    nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                    printInputArgs = printInputArgs, printResults = printResults)

    return(calculateCostValue(resTh, resOb))

  }

  ############################# FIT PROCEDURE ##################################
  if (fitInitialGrid) {

    minValsGrid   <- minVals
    maxValsGrid   <- maxVals
    startValsGrid <- startVals

    # which parameters to search within grid
    if (any(fixedGrid == 1)) {
      minValsGrid[fixedGrid == 1] <- startValsGrid[fixedGrid == 1]
      maxValsGrid[fixedGrid == 1] <- startValsGrid[fixedGrid == 1]
    }

    startValsGrid <- Map(seq, minValsGrid, maxValsGrid, length.out = fitInitialGridN)
    startValsGrid <- dplyr::distinct(expand.grid(Map(unique, startValsGrid)))
    message("Searching initial parameter gridspace: N = ", nrow(startValsGrid))

    # progress bar
    pb <- txtProgressBar(min = 0, max = nrow(startValsGrid), style = 3)
    progress <- function(n) {
      setTxtProgressBar(pb, n)
    }

    cl <- parallel::makeCluster(parallel::detectCores())
    doSNOW::registerDoSNOW(cl)
    costValue <- foreach::foreach(i = 1:nrow(startValsGrid), .packages = "DMCfun", .options.snow = list(progress = progress)) %dopar% {

      resTh <- dmcSim(amp = startValsGrid[i, 1], tau = startValsGrid[i, 2], mu = startValsGrid[i, 3],
                      bnds = startValsGrid[i, 4], resMean = startValsGrid[i, 5], resSD = startValsGrid[i, 6],
                      aaShape = startValsGrid[i, 7],
                      varSP = TRUE, spShape = startValsGrid[i, 8], spLim = c(-startValsGrid[i, 4], startValsGrid[i, 4]),
                      nTrl = 10000, stepDelta = stepDelta, stepCAF = stepCAF,
                      printInputArgs = printInputArgs, printResults = FALSE)
      return(calculateCostValue(resTh, resOb))

    }
    close(pb)
    stopCluster(cl)

    # adjust start, min and max vals for subsequent fit
    startVals <- as.numeric(startValsGrid[which.min(costValue), ])

    # move min/max values according to new start values
    # minVals <- startVals * 0.75
    # maxVals <- startVals * 1.25


  }

  # optimize
  fit <- optimr::optimr(par = startVals[!as.logical(fixedFit)], fn = minimizeCostValue,
                        prms = prms, fixedFit = fixedFit, resOb = resOb,
                        nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                        printInputArgs = printInputArgs, printResults = printResults,
                        method = "L-BFGS-B", lower = minVals[!as.logical(fixedFit)], upper = maxVals[!as.logical(fixedFit)],
                        control = list(parscale = parScale[!as.logical(fixedFit)]))

  prms[!as.logical(fixedFit)] <- fit$par
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
