#' @title dmcFitAgg
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions.
#'
#' @param resOb Observed data (see flankerData and simonTask for data format)
#' @param nTrl Number of trials to use within dmcSim.
#' @param startVals Starting values for to-be estimated parameters. This is a list with values specified individually for
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., startVals = list(amp = 20, tau = 200, mu = 0.5, bnds = 75, resMean = 300,
#' resSD = 30, aaShape = 2, spShape = 3, sigm = 4)).
#' @param minVals Minimum values for the to-be estimated parameters. This is a list with values specified individually for
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., minVals = list(amp = 10, tau = 5, mu = 0.1, bnds = 20, resMean = 200,
#' resSD = 5, aaShape = 1, spShape = 2, sigm = 1)).
#' @param maxVals Maximum values for the to-be estimated parameters. This is a list with values specified individually for
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., maxVals = list(amp = 40, tau = 300, mu = 1.0, bnds = 150, resMean = 800,
#' resSD = 100, aaShape = 3, spShape = 4, sigm = 10))
#' @param fixedFit Fix parameter to starting value. This is a list with bool values specified individually for
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., fixedFit = list(amp = F,  tau = F, mu = F, bnds = F, resMean = F,
#' resSD = F, aaShape = F, spShape = F, sigm = T))
#' @param fitInitialGrid TRUE/FALSE
#' @param fitInitialGridN 10 reduce if searching more than 1 initial parameter
#' @param fixedGrid Fix parameter for initial grid search.  This is a list with bool values specified individually for
#' amp, tau, mu, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., fixedGrid = list(amp = T, tau = F, mu = T, bnds = T, resMean = T,
#' resSD = T, aaShape = T, spShape = T, sigm = T))
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would result
#' in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#' @param stepDelta Step size for the Delta bins. For example, a step size of 5 would result
#' in 19 CAF bins positioned at 5, 10, 15, ... 85, 90, 95\%.
#' @param printInputArgs TRUE/FALSE
#' @param printResults TRUE/FALSE
#'
#' @return dmcfit
#'
#' The function returns a list with the relevant results from the fitting procedure. The list
#' is accessed with obj$name with the the following:
#' \item{obj$means}{Condition means for reaction time and error rate}
#' \item{obj$caf}{Accuracy per bin for compatible and incompatible trials}
#' \item{obj$delta}{Mean RT and compatibility effect per bin}
#' \item{obj$sim}{Individual trial data points (reaction times/error) and activation vectors from simulation}
#' \item{obj$par}{The fitted model parameters + final RMSE of the fit}
#'
#' @examples
#' \donttest{
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
#' # Example 3: Flanker data from Ulrich et al. (2015) with non-default
#' # start vals and some fixed values
#' fit <- dmcFitAgg(flankerData,
#'                  startVals = list(mu = 0.6, aaShape = 2.5),
#'                  fixedFit = list(mu = TRUE, aaShape = TRUE))
#'
#' # Example 4: Simulated Data (+ve going delta function)
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
                      nTrl            = 100000,
                      startVals       = list(),
                      minVals         = list(),
                      maxVals         = list(),
                      fixedFit        = list(),
                      fitInitialGrid  = TRUE,
                      fitInitialGridN = 10,      # reduce if grid search 3/4+ parameters
                      fixedGrid       = list(),  # default only initial search tau
                      stepCAF         = 20,
                      stepDelta       = 5,
                      printInputArgs  = TRUE,
                      printResults    = FALSE) {


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
                    varSP = TRUE, spShape = prms$spShape, sigm = prms$sigm, spLim = c(-prms$bnds, prms$bnds),
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
    
    # R check limits number of cores to 2 (https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions)
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && (chk == "true")) {
      num_cores <- 2L
    } else {
      num_cores <- parallel::detectCores() / 2
    }
  
    # Older code (pre June 2020) used doSNOW progress bar 
    # R CMD check --as-cran -->  doSNOW warning “superseded packages”
    # Use pbapply for progress bar
    # https://stackoverflow.com/questions/58473626/r-doparallel-progress-bar-to-monitor-finished-jobs
    
    pCostValue <- function(i){
      resTh <- dmcSim(amp = startValsGrid$amp[i], tau = startValsGrid$tau[i], mu = startValsGrid$mu[i],
                      bnds = startValsGrid$bnds[i], resMean = startValsGrid$resMean[i], resSD = startValsGrid$resSD[i],
                      aaShape = startValsGrid$aaShape[i], varSP = TRUE, spShape = startValsGrid$spShape[i],
                      sigm = startValsGrid$sigm[i],  spLim = c(-startValsGrid$bnds[i], startValsGrid$bnds[i]),
                      nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                      printInputArgs = TRUE, printResults = FALSE)
      return(calculateCostValue(resTh, resOb))
    }
    
    cl <- parallel::makeCluster(num_cores)
    invisible(parallel::clusterExport(cl = cl, varlist=c("dmcSim", "calculateCostValue")))
  
    # calculate initial cost values across grid starting values and find min
    costValue <- pbapply::pblapply(cl = cl, X = 1:nrow(startValsGrid), FUN = pCostValue)
    startVals <- startValsGrid[which.min(costValue), ]
    
    parallel::stopCluster(cl)

  } else {

    startVals <- unlist(startVals)

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
  prms <- as.list(prms)

  message(sprintf("RMSE: %.3f\n", fit$value))
  dmcfit <- dmcSim(amp = prms$amp, tau = prms$tau, mu = prms$mu,
                   bnds = prms$bnds, resMean = prms$resMean, resSD = prms$resSD,
                   aaShape = prms$aaShape, sigm = prms$sigm,
                   varSP = TRUE, spShape = prms$spShape, spLim = c(-prms$bnds, prms$bnds),
                   nTrl = nTrl, stepDelta = stepDelta, stepCAF = stepCAF,
                   printResults = TRUE)

  dmcfit$prms <- NULL
  dmcfit$par  <- prms
  dmcfit$par["RMSE"] <- fit$value

  class(dmcfit) <- "dmcfit"

  return(dmcfit)

}



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
#' \donttest{
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



#' @title mean.dmcfitvp
#'
#' @description Aggregated simulation results from dmcFitVPs.
#'
#' @param x Output from dmcFitVPs
#' @param ... pars
#'
#' @return dmcfit
#'
#' The function returns a list with the relevant aggregated results dmcFitVPs. The list
#' is accessed with obj$name and so on with the the following:
#' \item{obj$means}{means}
#' \item{obj$delta}{delta}
#' \item{obj$caf}{caf}
#' \item{obj$prms}{par}
#'
#' @examples
#' \donttest{
#' # Example 1: Fit individual data then aggregate
#' fitVPs <- dmcFitVPs(flankerData, nTrl = 1000, VP = c(2))
#' fitAgg <- mean(fitVPs)
#' plot(fitAgg, flankerData)
#' }
#'
#' @export
mean.dmcfitvp <- function(x, ...) {

  mergeLists <- function(lists, name) {
    return(lapply(c(name), function(x) do.call(rbind, lapply(lists, `[[`, x)))[[1]])
  }

  meanfit <- list()

  # summary
  meanfit$means <- mergeLists(x, "means") %>%
    dplyr::group_by(Comp) %>%
    dplyr::summarise(rtCor   = mean(rtCor),
                     sdCor   = mean(sdRtCor),
                     perErr  = mean(perErr),
                     rtErr   = mean(rtErr),
                     sdRtErr = mean(sdRtErr))

  # delta
  meanfit$delta <- mergeLists(x, "delta") %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarise(meanComp   = mean(meanComp),
                     meanIncomp = mean(meanIncomp),
                     meanBin    = mean(meanBin),
                     meanEffect = mean(meanEffect))

  # caf
  meanfit$caf <- mergeLists(x, "caf") %>%
    dplyr::group_by(Comp, bin) %>%
    dplyr::summarise(accPer = mean(accPer))

  # par
  meanfit$par <- as.data.frame(mergeLists(x, "par")) %>%
    dplyr::summarise(amp = mean(unlist(amp)),
                     tau = mean(unlist(tau)),
                     mu = mean(unlist(mu)),
                     bnds = mean(unlist(bnds)),
                     resMean = mean(unlist(resMean)),
                     resSD = mean(unlist(resSD)),
                     aaShape = mean(unlist(aaShape)),
                     spShape = mean(unlist(spShape)),
                     sigm = mean(unlist(sigm)),
                     RMSE = mean(unlist(RMSE)))

  class(meanfit) <- c("dmcfit")

  return(meanfit)

}



#' @title calculateCostValue
#'
#' @description Calculate cost value (fit) from combination of RT and error rate.
#'
#' @param resTh list containing caf values for comp/incomp conditions (nbins*2*3) and
#' delta values for comp/incomp conditions (nbins*5). See output from dmcSim (.$caf).
#' @param resOb list containing caf values for comp/incomp conditions (n*2*3) and
#' delta values for comp/incomp conditions (nbins*5). See output from dmcSim ($delta).
#'
#' @return cost value (RMSE)
#'
#' @examples
#' # Example 1:
#' resTh <- dmcSim()
#' resOb <- dmcSim()
#' cost  <- calculateCostValue(resTh, resOb)
#'
#' # Example 2:
#' resTh <- dmcSim()
#' resOb <- dmcSim(tau = 150)
#' cost  <- calculateCostValue(resTh, resOb)
#'
#' @export
calculateCostValue <- function(resTh, resOb) {

  n_rt  <- nrow(resTh$delta) * 2
  n_err <- nrow(resTh$caf)

  costCAF   <- sqrt((1/n_err) * sum((resTh$caf$accPer - resOb$caf$accPer)**2))
  costRT    <- sqrt((1/n_rt)  * sum((resTh$delta[c("meanComp", "meanIncomp")] - resOb$delta[c("meanComp", "meanIncomp")])**2))
  weightRT  <- n_rt / (n_rt + n_err)
  weightCAF <- (1 - weightRT) * 1500

  costValue <- (weightCAF * costCAF) + (weightRT * costRT)

  message(sprintf("RMSE: %.3f\n", costValue))

  return(costValue)

}
