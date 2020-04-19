#' @title dmcFitVPs
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions.
#'
#' @param resOb Observed data (see flankerData, simonData for data format)
#' @param nTrl Number of trials to use within dmcSim
#' @param startVals Starting values for to-be estimated parameters
#' @param minVals Minimum values for the to-be estimated parameters
#' @param maxVals Maximum values for the to-be estimated parameters
#' @param fixedFit Fix parameter to starting value
#' @param fitInitialGrid TRUE/FALSE (NB. overrides fitInitialTau)
#' @param fitInitialGridN 10
#' @param fixedGrid Fix parameter for initial grid search
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would result
#' in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#' @param stepDelta Step size for the Delta bins. For example, a step size of 5 would result
#' in 19 CAF bins positioned at 5, 10, 15, ... 85, 90, 95\%.
#' @param VP NULL (aggregated data across all participants) or integer for participant number
#' @param printInputArgs TRUE/FALSE
#' @param printResults TRUE/FALSE
#'
#' @return dmcfitvp List of dmcfit per participant fitted (see dmcFitAgg)
#'
#' @examples
#' \dontrun{
#' # Example 1: Flanker data from Ulrich et al. (2015)
#' fit <- dmcFitVPs(flankerData, nTrl = 1000, VP = c(1, 2))
#' plot(fit, flankerData, VP = 1)
#' plot(fit, flankerData, VP = 2)
#' summary(fit)
#' fitAgg <- mean(fit)
#' plot(fitAgg, flankerData)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFitVPs(simonData, nTrl = 1000, VP = c(1, 2))
#' plot(fit, simonData, VP = 1)
#' plot(fit, simonData, VP = 2)
#' summary(fit)
#' fitAgg <- mean(fit)
#' plot(fitAgg, simonData)
#' }
#'
#' @export
dmcFitVPs <- function(resOb,
                      nTrl             = 100000,
                      startVals        = list(), 
                      minVals          = list(), 
                      maxVals          = list(), 
                      fixedFit         = list(), 
                      fitInitialGrid   = TRUE,
                      fitInitialGridN  = 10,       # reduce if grid search 3/4+ parameters
                      fixedGrid        = list(),   # default only initial tau search
                      stepCAF          = 20,
                      stepDelta        = 5,
                      VP               = c(),
                      printInputArgs   = TRUE,
                      printResults     = FALSE) {

  if (length(VP) == 0) {
    VP = unique(resOb$summaryVP$VP)  # fit all individual VPs in data
  }
  
  # default parameter space
  defaultStartVals <- list(amp = 20, tau = 200, mu = 0.5, bnds =  75, resMean = 300, resSD =  30, aaShape = 2, spShape = 3, sigm =  4)
  defaultMinVals   <- list(amp = 10, tau =   5, mu = 0.1, bnds =  20, resMean = 200, resSD =  5,  aaShape = 1, spShape = 2, sigm =  1)
  defaultMaxVals   <- list(amp = 40, tau = 300, mu = 1.0, bnds = 150, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, sigm = 10)
  defaultFixedFit  <- list(amp = F,  tau = F,   mu = F,   bnds = F,   resMean = F,   resSD = F,   aaShape = F, spShape = F, sigm = T)
  defaultFixedGrid <- list(amp = T,  tau = F,   mu = T,   bnds = T,   resMean = T,   resSD = T,   aaShape = T, spShape = T, sigm = T)
  
  startVals <- modifyList(defaultStartVals, startVals)
  minVals   <- modifyList(defaultMinVals,   minVals)
  maxVals   <- modifyList(defaultMaxVals,   maxVals)
  fixedFit  <- modifyList(defaultFixedFit,  fixedFit)
  fixedGrid <- modifyList(defaultFixedGrid, fixedGrid)
 
  dmcfit <- vector("list", max(VP))
  for (vp in VP) {

    resObVP <- list(deltaAgg = resOb$deltaVP[resOb$deltaVP$VP == vp,],
                    cafAgg = resOb$cafVP[resOb$cafVP$VP == vp,])

    dmcfit[[vp]] <- dmcFitAgg(resObVP,
                              nTrl             = nTrl,
                              startVals        = startVals,
                              minVals          = minVals,
                              maxVals          = maxVals,
                              fixedFit         = fixedFit,
                              fitInitialGrid   = fitInitialGrid,
                              fitInitialGridN  = fitInitialGridN, # reduce if grid search 3/4+ parameters
                              fixedGrid        = fixedGrid,       # only fit tau
                              stepCAF          = stepCAF,
                              stepDelta        = stepDelta,
                              printInputArgs   = printInputArgs,
                              printResults     = printResults)

  }

  class(dmcfit) <- "dmcfitvp"
  
  return(dmcfit)

}
