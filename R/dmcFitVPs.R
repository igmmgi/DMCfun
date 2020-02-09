#' @title dmcFitVPs
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
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would result
#' in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#' @param stepDelta Step size for the Delta bins. For example, a step size of 5 would result
#' in 19 CAF bins positioned at 5, 10, 15, ... 85, 90, 95\%.
#' @param VP NULL (aggregated data across all participants) or integer for participant number
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
#' fit <- dmcFitVPs(flankerData1)
#' plot(fit, flankerData1, VP = 1)
#' summary(fit)
#' fitAgg <- mean(fit)
#' plot(fitAgg, flankerData1)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFitAgg(simonData1)
#' plot(fit, simonData1, VP = 1)
#' summary(fit)
#' fitAgg <- mean(fit)
#' plot(fitAgg, simonData1)
#'
#' }
#'
#' @export
dmcFitVPs <- function(resOb,
                      nTrl           = 50000,
                      startVals      = c(20, 100, 0.5,  75, 300,  30, 2, 3),
                      minVals        = c(10,   5, 0.1,  20, 200,   5, 1, 2),
                      maxVals        = c(30, 300, 1.0, 150, 800, 100, 3, 4),
                      fixed          = c(0, 0, 0, 0, 0, 0,  0, 0),
                      parScale       = startVals/min(startVals),
                      fitInitialTau  = TRUE,
                      stepCAF        = 20,
                      stepDelta      = 5,
                      VP             = c(),
                      printInputArgs = TRUE,
                      printResults   = FALSE) {

  if (length(VP) == 0) {
    # fit all individual VPs in data
    VP = unique(resOb$summaryVP$VP)
  }

  dmcfit <- vector("list", max(VP))
  for (vp in VP) {

    resObVP <- list(deltaAgg = resOb$deltaVP[resOb$deltaVP$VP == vp,],
                    cafAgg = resOb$cafVP[resOb$cafVP$VP == vp,])

    dmcfit[[vp]] <- dmcFitAgg(resObVP,
                              nTrl           = nTrl,
                              startVals      = startVals,
                              minVals        = minVals,
                              maxVals        = maxVals,
                              fixed          = fixed,
                              parScale       = parScale,
                              fitInitialTau  = fitInitialTau,
                              stepCAF        = stepCAF,
                              stepDelta      = stepDelta,
                              printInputArgs = printInputArgs,
                              printResults   = printResults)

  }

  class(dmcfit) <- "dmcfit"
  return(dmcfit)

}
