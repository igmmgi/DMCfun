#' @title dmcFitAgg
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions.
#'
#' @param resOb Observed data (see flankerData and simonTask for data format)
#' @param nTrl Number of trials to use within dmcSim.
#' @param startVals Starting values for to-be estimated parameters. This is a list with values specified individually for 
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigma (e.g., startVals = list(amp = 20, tau = 200, mu = 0.5, bnds = 75, resMean = 300, 
#' resSD = 30, aaShape = 2, spShape = 3, sigma = 4)).
#' @param minVals Minimum values for the to-be estimated parameters. This is a list with values specified individually for 
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigma (e.g., minVals = list(amp = 10, tau = 5, mu = 0.1, bnds = 20, resMean = 200, 
#' resSD = 5, aaShape = 1, spShape = 2, sigma = 1)). 
#' @param maxVals Maximum values for the to-be estimated parameters. This is a list with values specified individually for
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigma (e.g., maxVals = list(amp = 40, tau = 300, mu = 1.0, bnds = 150, resMean = 800, 
#' resSD = 100, aaShape = 3, spShape = 4, sigma = 10))
#' @param fixedFit Fix parameter to starting value. This is a list with bool values specified individually for 
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigma (e.g., fixedFit = list(amp = F,  tau = F,   mu = F, bnds = F, resMean = F,   
#' resSD = F, aaShape = F, spShape = F, sigma = T))
#' @param fitInitialGrid TRUE/FALSE
#' @param fitInitialGridN 10 reduce if searching more than 1 initial parameter
#' @param fixedGrid Fix parameter for initial grid search.  This is a list with bool values specified individually for 
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigma (e.g., fixedGrid = list(amp = T, tau = F, mu = T, bnds = T, resMean = T,   
#' resSD = T, aaShape = T, spShape = T, sigma = T))
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
#' fit <- dmcFitAgg(flankerData)  # only initial search tau
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
                      startVals        = list(), 
                      minVals          = list(), 
                      maxVals          = list(), 
                      fixedFit         = list(), 
                      fitInitialGrid   = TRUE,
                      fitInitialGridN  = 10,      # reduce if grid search 3/4+ parameters
                      fixedGrid        = list(),  # default only initial search tau
                      stepCAF          = 20,
                      stepDelta        = 5,
                      printInputArgs   = TRUE,
                      printResults     = FALSE) {
  
    
  # default parameter space
  defaultStartVals <- list(amp = 20, tau = 200, mu = 0.5, bnds =  75, resMean = 300, resSD =  30, aaShape = 2, spShape = 3, sigma =  4)
  defaultMinVals   <- list(amp = 10, tau =   5, mu = 0.1, bnds =  20, resMean = 200, resSD =  5,  aaShape = 1, spShape = 2, sigma =  1)
  defaultMaxVals   <- list(amp = 40, tau = 300, mu = 1.0, bnds = 150, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, sigma = 10)
  defaultFixedFit  <- list(amp = F,  tau = F,   mu = F,   bnds = F,   resMean = F,   resSD = F,   aaShape = F, spShape = F, sigma = T)
  defaultFixedGrid <- list(amp = T,  tau = F,   mu = T,   bnds = T,   resMean = T,   resSD = T,   aaShape = T, spShape = T, sigma = T)
  
  startVals <- modifyList(defaultStartVals, startVals)
  minVals   <- modifyList(defaultMinVals,   minVals)
  maxVals   <- modifyList(defaultMaxVals,   maxVals)
  fixedFit  <- modifyList(defaultFixedFit,  fixedFit)
  fixedGrid <- modifyList(defaultFixedGrid, fixedGrid)
    
  parScale  <- unlist(startVals) / min(unlist(startVals))
    
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
    prms <- as.list(pmax(unlist(prms), unlist(minVals)))
    prms <- as.list(pmin(unlist(prms), unlist(maxVals)))
    
    resTh <- dmcSim(amp = prms$amp, tau = prms$tau, mu = prms$mu, bnds = prms$bnds,
                    resMean = prms$resMean, resSD = prms$resSD, aaShape = prms$aaShape,
                    varSP = TRUE, spShape = prms$spShape, sigma = prms$sigma, spLim = c(-prms$bnds, prms$bnds),
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
    if (TRUE %in% fixedGrid) {
      minValsGrid[fixedGrid == T] = startValsGrid[fixedGrid == T]
      maxValsGrid[fixedGrid == T] = startValsGrid[fixedGrid == T]
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
                                    resTh <- dmcSim(amp = startValsGrid$amp[i], tau = startValsGrid$tau[i], mu = startValsGrid$mu[i],
                                                    bnds = startValsGrid$bnds[i], resMean = startValsGrid$resMean[i], resSD = startValsGrid$resSD[i],
                                                    aaShape = startValsGrid$aaShape[i], varSP = TRUE, spShape = startValsGrid$spShape[i],
                                                    sigma = startValsGrid$sigma[i],  spLim = c(-startValsGrid$bnds[i], startValsGrid$bnds[i]),
                                                    nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                                                    printInputArgs = FALSE, printResults = FALSE)
                                    return(calculateCostValue(resTh, resOb))
                                  }
    close(pb)
    stopCluster(cl)
    
    startVals <- startValsGrid[which.min(costValue), ]
    
  }

  # optimize
  fit <- optimr::optimr(par = startVals[!as.logical(fixedFit)], fn = minimizeCostValue,
                        prms = prms, fixedFit = fixedFit, resOb = resOb,
                        nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF, minVals = minVals, maxVals = maxVals,
                        printInputArgs = printInputArgs, printResults = printResults,
                        method = "Nelder-Mead", 
                        control = list(parscale = parScale[!as.logical(fixedFit)]))
    
  prms[!as.logical(fixedFit)] <- fit$par
  
  # bounds check
  prms <- pmax(unlist(prms), unlist(minVals))
  prms <- pmin(unlist(prms), unlist(maxVals))
  if (any(prms == unlist(minVals)) || any(prms == unlist(maxVals))) {
    warning("Parameter estimates at minVals/maxVals bounds!")
  }
  prms    <- as.list(prms)
  fit$par <- prms 
  fit$par["RMSE"] <- fit$value
  
  cat(sprintf("RMSE: %.3f\n", fit$value))
  resTh <- dmcSim(amp = prms$amp, tau = prms$tau, mu = prms$mu,
                  bnds = prms$bnds, resMean = prms$resMean, resSD = prms$resSD,
                  aaShape = prms$aaShape, sigma = prms$sigma,
                  varSP = TRUE, spShape = prms$spShape, spLim = c(-prms$bnds, prms$bnds),
                  nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                  printResults = TRUE)
  resTh$prms[1:9] <- prms
  
  dmcfit        <- list(resTh, fit)
  class(dmcfit) <- "dmcfit"
  
  return(dmcfit)
  
}

