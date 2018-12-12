#' @title dmcFitAgg
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions.
#'
#' @param resOb Observed data (see flankerData1, flankerData2,
#' simonTask1 for data format)
#' @param nTrl Number of trials to use within dmcSim
#' @param method L-BFGS-B (default)
#' @param startVals Starting values for to-be estimated parameters
#' @param minVals Minimum values for the to-be estimated parameters
#' @param maxVals Maximum values for the to-be estimated parameters
#' @param parScale Scalling values for the to-be estimated parameters
#' @param fitInitialTau TRUE/FALSE
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
#' # Example 5: Simulated Data
#' dat <- createDF(nVP = 50, nTrl = 100,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp"), vals = c(510, 100, 100)),
#'                            list(c("Comp:incomp"), vals = c(540, 100, 120))),
#'                  Error = list(list(c("Comp:comp"), vals = c(3, 2, 2, 1, 1)),
#'                             list(c("Comp:incomp"), vals = c(25, 3, 2, 1, 1))))
#' datOb <- dmcObservedData(dat)
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
                      method         = "L-BFGS-B",
                      startVals      = c(20, 100, 0.5,  75, 300,  30, 3),
                      minVals        = c(10,   5, 0.1,  20, 200,   5, 2),
                      maxVals        = c(30, 300, 1.0, 150, 800, 100, 4),
                      parScale       = startVals/min(startVals),
                      fitInitialTau  = TRUE,
                      stepCAF        = 20,
                      stepDelta      = 5,
                      printInputArgs = TRUE,
                      printResults   = FALSE) {

  # check observed data contains correct number of delta/CAF bins
  if (nrow(resOb$delta) != length(seq(stepDelta, 100 - stepDelta, stepDelta))) {
    stop("Number of delta bins in observed data and nDelta bins are not equal!")
  }
  if ((nrow(resOb$caf)/2) != length(seq(0, 100 - stepCAF, stepCAF))) {
    stop("Number of CAF bins in observed data and nCAF bins are not equal!")
  }

  # cost value is combination of RT and error rate data
  calcCostValue <- function(resTh, resOb) {

    n_rt  <- nrow(resTh$delta)
    n_err <- nrow(resTh$caf)

    costCAF <- sqrt((1/n_rt)  * sum((resTh$caf$accPer - resOb$caf$accPer)**2))
    costRT  <- sqrt((1/n_err) * sum((resTh$delta[c("meanComp", "meanIncomp")] - resOb$delta[c("meanComp", "meanIncomp")])**2))

    costValue <- (((1 - (2*n_rt) / (2*n_rt + 2*n_err)) * 1500) * costCAF) + costRT
    cat(sprintf("RMSE: %.3f\n", costValue))

    return(costValue)

  }

  # function to minimise
  minimizeCostValue <- function(x, resOb, nTrl, stepDelta, stepCAF, printInputArgs, printResults) {

    resTh <- dmcSim(amp = x[1], tau = x[2], mu = x[3], bnds = x[4], resMean = x[5], resSD = x[6],
                    varSP = TRUE, spShape = x[7], spLim = c(-x[4], x[4]),
                    nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                    printInputArgs = printInputArgs, printResults = printResults)

    return(calcCostValue(resTh, resOb))

  }

  ############################# FIT PROCEDURE ##################################
  if (fitInitialTau) {
    lowestCostValue <- Inf
    for (t in seq(15, 150, 5)) {
      resTh <- dmcSim(amp = startVals[1], tau = t, mu = startVals[3],
                      bnds = startVals[4], startVals[5], startVals[6],
                      varSP = TRUE, spShape = startVals[7], spLim = c(-startVals[4], startVals[4]),
                      nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                      printInputArgs = printInputArgs, printResults = FALSE)
      costValue <- calcCostValue(resTh, resOb)
      if (costValue < lowestCostValue) {
        lowestCostValue <- costValue
        startTau <- t
      }
    }
    startVals[2] <- startTau
    minVals[2]   <- max(5, startTau - 20)
    maxVals[2]   <- min(300, startTau + 20)
  }

  fit <- optimr::optimr(par = startVals, fn = minimizeCostValue, resOb = resOb,
                        nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                        printInputArgs = printInputArgs, printResults = printResults,
                        method = method, lower = minVals, upper = maxVals,
                        control = list(parscale = parScale))

  resTh <- dmcSim(amp = fit[["par"]][1], tau = fit[["par"]][2], mu = fit[["par"]][3],
                  bnds = fit[["par"]][4], resMean = fit[["par"]][5], resSD = fit[["par"]][6],
                  varSP = TRUE, spShape = fit[["par"]][7], spLim = c(-fit[["par"]][4], fit[["par"]][4]),
                  nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                  printResults = FALSE)

  cat(sprintf("RMSE: %.3f\n", fit[2]$value))

  dmcfit <- list(resTh, fit)
  class(dmcfit) <- c("dmcfit")

  return(dmcfit)

}
