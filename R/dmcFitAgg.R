#' @title dmcFitAgg
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions.
#'
#' @param resOb Observed data (see flankerData and simonTask for data format)
#' @param nTrl Number of trials to use within dmcSim
#' @param startVals Starting values for to-be estimated parameters. This needs to be vector of length 9
#' with the following order: amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigma
#' @param minVals Minimum values for the to-be estimated parameters (see startVals)
#' @param maxVals Maximum values for the to-be estimated parameters (see startVals)
#' @param fixedFit Fix parameter to starting value (bool vector of length 9)
#' @param parScale Scaling values for the to-be estimated parameters
#' @param fitInitialGrid TRUE/FALSE
#' @param fitInitialGridN 10 reduce if searching more than 1 initial parameter
#' @param fixedGrid Fix parameter for initial grid search (bool vector of length 9)
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
#' fit <- dmcFitAgg(flankerData, nTrl = 5000)  # only initial search tau
#' plot(fit, flankerData)
#' summary(fit)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFitAgg(simonData)    # only initial search tau
#' plot(fit, simonData)
#' summary(fit)
#'
#' # Example 3: Simulated Data (+ve going delta function)
#' dat <- createDF(nVP = 20, nTrl = 500, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(510, 100, 100),
#'                            "Comp_incomp" = c(540, 130, 85)),
#'                  Error = list("Comp_comp"   = c(4, 3, 2, 1, 1),
#'                               "Comp_incomp" = c(20, 4, 3, 1, 1)))
#' datOb <- dmcObservedData(dat, columns = c("VP", "Comp", "RT", "Error"))
#' plot(datOb)
#' fit <- dmcFitAgg(datOb, nTrl = 5000)
#' plot(fit, datOb)
#' summary(fit)
#' }
#'
#' @export
dmcFitAgg <- function(resOb,
                      nTrl             = 100000,
                      startVals        = c(20, 100, 0.5,  75, 300,  30, 2, 3, 4),
                      minVals          = c(10,   5, 0.1,  20, 200,   5, 1, 2, 1),
                      maxVals          = c(40, 300, 1.0, 150, 800, 100, 3, 4, 10),
                      fixedFit         = c( 0,   0, 0,     0,   0,   0, 0, 0, 1),
                      parScale         = startVals/min(startVals),
                      fitInitialGrid   = TRUE,
                      fitInitialGridN  = 10,                                       # reduce if grid search 3/4+ parameters
                      fixedGrid        = c( 1,   0, 1,     1,   1,   1, 1, 1, 1),  # only initial search tau
                      stepCAF          = 20,
                      stepDelta        = 5,
                      printInputArgs   = TRUE,
                      printResults     = FALSE) {

  prms <- startVals

  # check observed data contains correct number of delta/CAF bins
  if (nrow(resOb$delta) != sum(!seq(stepDelta, 100, stepDelta) %in% 100)) {
    stop("Number of delta bins in observed data and nDelta bins are not equal!")
  }
  if ((nrow(resOb$caf)/2) != length(seq(0, 100 - stepCAF, stepCAF))) {
    stop("Number of CAF bins in observed data and nCAF bins are not equal!")
  }

  # function to minimise
  minimizeCostValue <- function(x, prms, fixedFit, resOb, nTrl, stepDelta, stepCAF, minVals, maxVals, printInputArgs, printResults) {

    prms[!as.logical(fixedFit)] <- x

    # implement bounds
    prms <- pmax(prms, minVals)
    prms <- pmin(prms, maxVals)

    resTh <- dmcSim(amp = prms[1], tau = prms[2], mu = prms[3], bnds = prms[4],
                    resMean = prms[5], resSD = prms[6], aaShape = prms[7],
                    varSP = TRUE, spShape = prms[8], sigma = prms[9], spLim = c(-prms[4], prms[4]),
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

     # R check limits number of cores to 2 (https://cran.r-project.org/web/packages/policies.html)
     chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
     if (nzchar(chk) && chk == "TRUE") {
       num_cores <- 2
     } else {
       num_cores <- parallel::detectCores() / 2
     }

     cl <- parallel::makeCluster(num_cores)
     doSNOW::registerDoSNOW(cl)
     costValue <- foreach::foreach(i = 1:nrow(startValsGrid),
                                   .packages = "DMCfun",
                                   .options.snow = list(progress = progress)) %dopar% {
       resTh <- dmcSim(amp = startValsGrid[i, 1], tau = startValsGrid[i, 2], mu = startValsGrid[i, 3],
                       bnds = startValsGrid[i, 4], resMean = startValsGrid[i, 5], resSD = startValsGrid[i, 6],
                       aaShape = startValsGrid[i, 7], varSP = TRUE, spShape = startValsGrid[i, 8],
                       sigma = startValsGrid[i, 9],  spLim = c(-startValsGrid[i, 4], startValsGrid[i, 4]),
                       nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                       printInputArgs = FALSE, printResults = FALSE)
       return(calculateCostValue(resTh, resOb))
     }
     close(pb)
     stopCluster(cl)

    startVals <- as.numeric(startValsGrid[which.min(costValue), ])

  }

  # optimize
  fit <- optimr::optimr(par = startVals[!as.logical(fixedFit)], fn = minimizeCostValue,
                        prms = prms, fixedFit = fixedFit, resOb = resOb,
                        nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF, minVals = minVals, maxVals = maxVals,
                        printInputArgs = printInputArgs, printResults = printResults,
                        method = "Nelder-Mead",
                        control = list(parscale = parScale[!as.logical(fixedFit)]))

  prms[!as.logical(fixedFit)] <- fit$par

  # implement bounds
  prms <- pmax(prms, minVals)
  prms <- pmin(prms, maxVals)
  if (any(prms == minVals) || any(prms == maxVals)) {
    warning("Parameter estimates at minVals/maxVals bounds!")
  }

  fit$par <- prms

  cat(sprintf("RMSE: %.3f\n", fit$value))
  resTh <- dmcSim(amp = prms[1], tau = prms[2], mu = prms[3],
                  bnds = prms[4], resMean = prms[5], resSD = prms[6],
                  aaShape = prms[7], sigma = prms[9],
                  varSP = TRUE, spShape = prms[8], spLim = c(-prms[4], prms[4]),
                  nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                  printResults = TRUE)
  resTh$prms[1:9] <- prms

  dmcfit        <- list(resTh, fit)
  class(dmcfit) <- "dmcfit"

  return(dmcfit)

}
