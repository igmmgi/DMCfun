#' @title dmcFit: Fit DMC to aggregated data using R-package optimx (Nelder-Mead)
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions using the R-package optimx (Nelder-Mead).
#'
#' @param resOb Observed data (see flankerData and simonTask for data format) and the function dmcObservedData to create
#' the required input from either an R data frame or external *.txt/*.csv files
#' @param nTrl Number of trials to use within dmcSim.
#' @param startVals Starting values for to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., startVals = list(amp = 20, tau = 200,
#' drc = 0.5, bnds = 75, resMean = 300, resSD = 30, aaShape = 2, spShape = 3, spBias = 0, sigm = 4)).
#' @param minVals Minimum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., minVals = list(amp = 10, tau = 5, drc = 0.1,
#' bnds = 20, resMean = 200, resSD = 5, aaShape = 1, spShape = 2, spBias = -20, sigm = 1)).
#' @param maxVals Maximum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., maxVals = list(amp = 40, tau = 300, drc = 1.0,
#' bnds = 150, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 20, sigm = 10))
#' @param fixedFit Fix parameter to starting value. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., fixedFit = list(amp = F, tau = F, drc = F,
#' bnds = F, resMean = F, resSD = F, aaShape = F, spShape = F, spBias = T, sigm = T))
#' @param fitInitialGrid TRUE/FALSE
#' @param fitInitialGridN 10 reduce if searching more than ~2/3 initial parameters
#' @param fixedGrid Fix parameter for initial grid search. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., fixedGrid = list(amp = T, tau = F, drc = T,
#' bnds = T, resMean = T, resSD = T, aaShape = T, spShape = T, spBias = T, sigm = T))
#' @param nCAF Number of CAF bins.
#' @param nDelta Number of delta bins.
#' @param pDelta alternative to nDelta by directly specifying required percentile values
#' @param tDelta type of delta calculation (1=direct percentiles points, 2=percentile bounds (tile) averaging)
#' @param costFunction Cost function to minimise root mean square error ("RMSE": default),
#' squared percentage error ("SPE"), or likelihood-ratio chi-square statistic (GS)
#' @param rtMax limit on simulated RT (decision + non-decisional component)
#' @param spDist starting point (sp) distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param drDist drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape drift rate (dr) shape parameter
#' @param drLim drift rate (dr) range
#' @param printInputArgs TRUE/FALSE
#' @param printResults TRUE/FALSE
#' @param optimxControl Control parameters passed to optimx (see optimx Details section)
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
#' fit <- dmcFit(flankerData)  # only initial search tau
#' plot(fit, flankerData)
#' summary(fit)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFit(simonData)    # only initial search tau
#' plot(fit, simonData)
#' summary(fit)
#'
#' # Example 3: Flanker data from Ulrich et al. (2015) with non-default
#' # start vals and some fixed values
#' fit <- dmcFit(flankerData,
#'               startVals = list(drc = 0.6, aaShape = 2.5),
#'               fixedFit = list(drc = TRUE, aaShape = TRUE))
#'
#' # Example 4: Simulated Data (+ve going delta function)
#' dat <- createDF(nSubjects = 20, nTrl = 500, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(510, 100, 100),
#'                            "Comp_incomp" = c(540, 130, 85)),
#'                  Error = list("Comp_comp"   = c(4, 3, 2, 1, 1),
#'                               "Comp_incomp" = c(20, 4, 3, 1, 1)))
#' datOb <- dmcObservedData(dat, columns = c("Subject", "Comp", "RT", "Error"))
#' plot(datOb)
#' fit <- dmcFit(datOb, nTrl = 5000)
#' plot(fit, datOb)
#' summary(fit)
#' }
#'
#' @export
dmcFit <- function(resOb,
                   nTrl            = 100000,
                   startVals       = list(),
                   minVals         = list(),
                   maxVals         = list(),
                   fixedFit        = list(),
                   fitInitialGrid  = TRUE,
                   fitInitialGridN = 10,      # reduce if grid search 3/4+ parameters
                   fixedGrid       = list(),  # default only initial search tau
                   nCAF            = 5,
                   nDelta          = 19,
                   pDelta          = vector(),
                   tDelta          = 1,
                   spDist          = 1,
                   drDist          = 0,
                   drShape         = 3,
                   drLim           = c(0.1, 0.7),
                   rtMax           = 5000,
                   costFunction    = "RMSE",
                   printInputArgs  = TRUE,
                   printResults    = FALSE,
                   optimxControl   = list()) {

  # default parameter space
  defaultStartVals <- list(amp = 20, tau = 200, drc = 0.5, bnds =  75, resMean = 300, resSD =  30, aaShape = 2, spShape = 3, spBias =   0, sigm =  4)
  defaultMinVals   <- list(amp =  0, tau =   5, drc = 0.1, bnds =  20, resMean = 200, resSD =  5,  aaShape = 1, spShape = 2, spBias = -20, sigm =  1)
  defaultMaxVals   <- list(amp = 40, tau = 300, drc = 1.0, bnds = 150, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias =  20, sigm = 10)
  defaultFixedFit  <- list(amp = F,  tau = F,   drc = F,   bnds = F,   resMean = F,   resSD = F,   aaShape = F, spShape = F, spBias = T,   sigm = T)
  defaultFixedGrid <- list(amp = T,  tau = F,   drc = T,   bnds = T,   resMean = T,   resSD = T,   aaShape = T, spShape = T, spBias = T,   sigm = T)


  startVals <- modifyList(defaultStartVals, startVals)
  startVals <- lapply(startVals, function(x) ifelse(x == 0, .Machine$double.xmin, x))
  if (!fitInitialGrid) {
    startVals <- unlist(startVals)
  }
  minVals   <- modifyList(defaultMinVals,   minVals)
  maxVals   <- modifyList(defaultMaxVals,   maxVals)
  fixedFit  <- modifyList(defaultFixedFit,  fixedFit)
  fixedGrid <- modifyList(defaultFixedGrid, fixedGrid)

  parScale  <- unlist(startVals) / min(unlist(startVals) + 1)
  prms      <- startVals

  # default optimx control parameters
  defaultOptimxControl <- list(parscale = parScale[!as.logical(fixedFit)], maxit = 500)
  optimxControl        <- modifyList(defaultOptimxControl, optimxControl)

  # check observed data contains correct number of delta/CAF bins
  if (nrow(resOb$delta) != nDelta) {
    stop("Number of delta bins in observed data and nDelta bins are not equal!")
  }
  if ((nrow(resOb$caf)) != nCAF) {
    stop("Number of CAF bins in observed data and nCAF bins are not equal!")
  }

  # which cost function?
  if (is.character(costFunction)) {
    if (costFunction == "RMSE") {
      calculateCostValue <- calculateCostValueRMSE
    } else if (costFunction == "SPE") {
      calculateCostValue <- calculateCostValueSPE
    } else if (costFunction == "GS") {
      calculateCostValue <- calculateCostValueGS
    }
  } else if (is.function(costFunction)) {
    calculateCostValue <- costFunction
  }

  ############################# FIT PROCEDURE ##################################
  if (fitInitialGrid) {

    minValsGrid   <- minVals
    maxValsGrid   <- maxVals
    startValsGrid <- startVals

    # which parameters to search within grid
    if (TRUE %in% fixedGrid) {
      minValsGrid[fixedGrid == T] <- startValsGrid[fixedGrid == T]
      maxValsGrid[fixedGrid == T] <- startValsGrid[fixedGrid == T]
    }

    startValsGrid <- Map(seq, minValsGrid, maxValsGrid, length.out = fitInitialGridN)
    startValsGrid <- dplyr::distinct(expand.grid(Map(unique, startValsGrid)))
    message("Searching initial parameter gridspace: N = ", nrow(startValsGrid), ", with ", nTrl, " trials per simulation.")

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

    pCostValue <- function(i) {
      resTh <- dmcSim(
        amp = startValsGrid$amp[i], tau = startValsGrid$tau[i], drc = startValsGrid$drc[i], bnds = startValsGrid$bnds[i],
        resMean = startValsGrid$resMean[i], resSD = startValsGrid$resSD[i], aaShape = startValsGrid$aaShape[i],
        spShape = startValsGrid$spShape[i], spBias = startValsGrid$spBias[i], sigm = startValsGrid$sigm[i],
        spLim = c(-startValsGrid$bnds[i], startValsGrid$bnds[i]), spDist = spDist,
        drDist = drDist, drShape = drShape, drLim = drLim,
        rtMax = rtMax, nTrl = nTrl, nDelta = nDelta, pDelta = pDelta, tDelta = tDelta, nCAF = nCAF,
        printInputArgs = TRUE, printResults = FALSE
      )
      return(calculateCostValue(resTh, resOb))
    }

    cl <- parallel::makeCluster(num_cores)
    invisible(parallel::clusterExport(cl = cl, varlist = c("dmcSim", "calculateCostValue"), envir = environment()))

    # calculate initial cost values across grid starting values and find min
    costValue <- pbapply::pblapply(cl = cl, X = 1:nrow(startValsGrid), FUN = pCostValue)
    startVals <- startValsGrid[which.min(costValue), ]

    parallel::stopCluster(cl)

  }

  # optimize
  fit <- optimx::optimx(par = as.numeric(startVals[!as.logical(fixedFit)]), fn = minimizeCostValue,
                        costFunction = calculateCostValue, prms = prms, fixedFit = fixedFit, resOb = resOb,
                        nTrl = nTrl, nDelta = nDelta, pDelta = pDelta, tDelta = tDelta, nCAF = nCAF,
                        spDist = spDist, drDist = drDist, drShape = drShape, drLim = drLim,
                        rtMax = rtMax, minVals = minVals, maxVals = maxVals,
                        printInputArgs = printInputArgs, printResults = printResults,
                        method = "Nelder-Mead", control = optimxControl)

  prms[!as.logical(fixedFit)] <- fit[1:attr(fit, "npar")]

  # bounds check
  prms <- pmax(unlist(prms), unlist(minVals))
  prms <- pmin(unlist(prms), unlist(maxVals))
  if (any(prms == unlist(minVals)) || any(prms == unlist(maxVals))) {
    warning("Parameter estimates at minVals/maxVals bounds!")
  }
  prms <- as.list(prms)

  dmcfit <- dmcSim(amp = prms$amp, tau = prms$tau, drc = prms$drc, bnds = prms$bnds,
                   resMean = prms$resMean, resSD = prms$resSD, aaShape = prms$aaShape,
                   spDist = spDist, spBias = prms$spBias, spShape = prms$spShape, spLim = c(-prms$bnds, prms$bnds),
                   sigm = prms$sigm, drDist = drDist, drShape = drShape, drLim = drLim,
                   rtMax = rtMax, nTrl = nTrl, nDelta = nDelta, pDelta = pDelta, tDelta = tDelta, nCAF = nCAF,
                   printResults = TRUE)

  # fitted parameters
  dmcfit$prms <- NULL  # TO DO: Would this be useful to keep or is it only redundant?
  dmcfit$par  <- prms
  dmcfit$par["cost"] <- fit$value

  if (costFunction == "GS") {
    dmcfit$par["BIC"] = fit$value + (sum(unlist(fixedFit) == FALSE))*log(sum(resOb$data$outlier == 0))
  }

  class(dmcfit) <- "dmcfit"

  return(dmcfit)

}



#' @title dmcFitDE: Fit DMC to aggregated data using R-package DEoptim
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions using the R-package DEoptim.
#'
#' @param resOb Observed data (see flankerData and simonTask for data format)
#' @param nTrl Number of trials to use within dmcSim.
#' @param minVals Minimum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., minVals = list(amp = 10, tau = 5, drc = 0.1,
#' bnds = 20, resMean = 200, resSD = 5, aaShape = 1, spShape = 2, spBias = -20, sigm = 1)).
#' @param maxVals Maximum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., maxVals = list(amp = 40, tau = 300, drc = 1.0,
#' bnds = 150, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 20, sigm = 10))
#' @param fixedFit Fix parameter to starting value. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., fixedFit = list(amp = F,  tau = F, drc = F,
#' bnds = F, resMean = F, resSD = F, aaShape = F, spShape = F, spBias = T, sigm = T))
#' @param nCAF Number of CAF bins.
#' @param nDelta Number of delta bins.
#' @param pDelta Alternative to nDelta by directly specifying required percentile values
#' @param tDelta Type of delta calculation (1 = percentile, 2 = tile average)
#' @param costFunction Cost function to minimise root mean square error ("RMSE": default) or
#' squared percentage error ("SPE")
#' @param rtMax limit on simulated RT (decision + non-decisional component)
#' @param spDist starting point distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param drDist drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape drift rate (dr) shape parameter
#' @param drLim drift rate (dr) range
#' @param deControl Additional control parameters passes to DEoptim (see DEoptim.control)
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
#' fit <- dmcFitDE(flankerData)
#' plot(fit, flankerData)
#' summary(fit)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFitDE(simonData, nTrl = 20000)
#' plot(fit, simonData)
#' summary(fit)
#' }
#'
#' @export
dmcFitDE <- function(resOb,
                     nTrl         = 100000,
                     minVals      = list(),
                     maxVals      = list(),
                     fixedFit     = list(),
                     nCAF         = 5,
                     nDelta       = 19,
                     pDelta       = vector(),
                     tDelta       = 1,
                     costFunction = "RMSE",
                     spDist       = 1,
                     drDist       = 0,
                     drShape      = 3,
                     drLim        = c(0.1, 0.7),
                     rtMax        = 5000,
                     deControl    = list()) {

  # default parameter space
  defaultMinVals  <- list(amp = 10, tau =   5, drc = 0.1, bnds =  20, resMean = 200, resSD =  5,  aaShape = 1, spShape = 2, spBias = 0, sigm = 4)
  defaultMaxVals  <- list(amp = 40, tau = 300, drc = 1.0, bnds = 150, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 0, sigm = 4)
  defaultFixedFit <- list(amp = F,  tau = F,   drc = F,   bnds = F,   resMean = F,   resSD = F,   aaShape = F, spShape = F, spBias = T, sigm = T)

  minVals  <- modifyList(defaultMinVals,  minVals)
  maxVals  <- modifyList(defaultMaxVals,  maxVals)
  fixedFit <- modifyList(defaultFixedFit, fixedFit)

  prms <- (unlist(minVals) + unlist(maxVals)) / 2  # start in middle of min/max vals

  # check observed data contains correct number of delta/CAF bins
  if (nrow(resOb$delta) != nDelta) {
    stop("Number of delta bins in observed data and nDelta bins are not equal!")
  }
  if ((nrow(resOb$caf)) != nCAF) {
    stop("Number of CAF bins in observed data and nCAF bins are not equal!")
  }

  # which cost function?
  if (is.character(costFunction)) {
    if (costFunction == "RMSE") {
      calculateCostValue <- calculateCostValueRMSE
    } else if (costFunction == "SPE") {
      calculateCostValue <- calculateCostValueSPE
    }
  } else if (is.function(costFunction)) {
    calculateCostValue <- costFunction
  }

  # R check limits number of cores to 2 (https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions)
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(chk) && (chk == "true")) {
    num_cores <- 2L
  } else {
    num_cores <- parallel::detectCores() / 2
  }
  cl <- parallel::makeCluster(num_cores)
  invisible(parallel::clusterExport(cl = cl, varlist = c("dmcSim", "calculateCostValue"), envir = environment()))

  defaultControl <- list(VTR = 0,  strategy = 1, NP = 100, itermax  = 200, trace = 1, cluster = cl)
  deControl      <- modifyList(defaultControl,  deControl)

  fit <- DEoptim::DEoptim(fn = minimizeCostValue,
                          lower = unlist(minVals[!as.logical(fixedFit)]),
                          upper = unlist(maxVals[!as.logical(fixedFit)]),
                          costFunction = calculateCostValue,
                          prms = prms,
                          fixedFit = fixedFit,
                          minVals = minVals,
                          maxVals = maxVals,
                          resOb = resOb,
                          nTrl = nTrl,
                          nDelta = nDelta,
                          nCAF = nCAF,
                          pDelta = pDelta,
                          tDelta = tDelta,
                          spDist = spDist,
                          drDist = drDist,
                          drShape = drShape,
                          drLim = drLim,
                          rtMax = rtMax,
                          printInputArgs = FALSE,
                          printResults = FALSE,
                          control = deControl)

  parallel::stopCluster(cl)

  prms[!as.logical(fixedFit)] <- as.list(fit$optim$bestmem)

  # bounds check
  prms <- pmax(unlist(prms), unlist(minVals))
  prms <- pmin(unlist(prms), unlist(maxVals))
  if (any(prms[!as.logical(fixedFit)] == unlist(minVals[!as.logical(fixedFit)]))
      || any(prms[!as.logical(fixedFit)] == unlist(maxVals[!as.logical(fixedFit)]))) {
    warning("Parameter estimates at minVals/maxVals bounds!")
  }
  prms <- as.list(prms)

  cat("\n")

  dmcfit <- dmcSim(amp = prms$amp, tau = prms$tau, drc = prms$drc, bnds = prms$bnds,
                   resMean = prms$resMean, resSD = prms$resSD, aaShape = prms$aaShape,
                   spDist = spDist, spBias = prms$spBias, spShape = prms$spShape, spLim = c(-prms$bnds, prms$bnds),
                   sigm = prms$sigm, drDist = drDist, drShape = drShape, drLim = drLim,
                   rtMax = rtMax, nTrl = nTrl, nDelta = nDelta, pDelta = pDelta, tDelta = tDelta, nCAF = nCAF,
                   printResults = TRUE)

  # fitted parameters
  dmcfit$prms <- NULL  # TO DO: Would this be useful to keep or is it only redundant?
  dmcfit$par  <- prms
  dmcfit$par["RMSE"] <- fit$optim$bestval

  class(dmcfit) <- "dmcfit"

  return(dmcfit)

}


#' @title dmcFitSubject: Fit DMC to aggregated data using R-package optimx (Nelder-Mead)
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions.
#'
#' @param resOb Observed data (see flankerData, simonData for data format)
#' @param nTrl Number of trials to use within dmcSim
#' @param startVals Starting values for to-be estimated parameters (see dmcFit)
#' @param minVals Minimum values for the to-be estimated parameters (see dmcFit)
#' @param maxVals Maximum values for the to-be estimated parameters (see dmcFit)
#' @param fixedFit Fix parameter to starting value (see dmcFit)
#' @param fitInitialGrid TRUE/FALSE
#' @param fitInitialGridN 10
#' @param fixedGrid Fix parameter for initial grid search
#' @param nCAF Number of CAF bins.
#' @param nDelta Number of delta bins.
#' @param pDelta Alternative to nDelta by directly specifying required percentile values
#' @param tDelta Type of delta calculation (1 = percentile, 2 = tile average)
#' @param costFunction Cost function to minimise root mean square error ("RMSE": default) or
#' squared percentage error ("SPE")
#' @param spDist starting point distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param drDist drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape drift rate (dr) shape parameter
#' @param drLim drift rate (dr) range
#' @param rtMax limit on simulated RT (decision + non-decisional component)
#' @param subjects NULL (aggregated data across all subjects) or integer for subject number
#' @param printInputArgs TRUE/FALSE
#' @param printResults TRUE/FALSE
#' @param optimxControl Control parameters passed to optimx (see optimx Details section)
#'
#' @return dmcfit_subject List of dmcfit per subject fitted (see dmcFit)
#'
#' @examples
#' \donttest{
#' # Example 1: Flanker data from Ulrich et al. (2015)
#' fit <- dmcFitSubject(flankerData, nTrl = 1000, subjects = c(1, 2))
#' plot(fit, flankerData, subject = 1)
#' plot(fit, flankerData, subject = 2)
#' summary(fit)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFitSubject(simonData, nTrl = 1000, subject = c(1, 2))
#' plot(fit, simonData, subject = 1)
#' plot(fit, simonData, subject = 2)
#' summary(fit)
#' }
#'
#' @export
dmcFitSubject <- function(resOb,
                          nTrl            = 100000,
                          startVals       = list(),
                          minVals         = list(),
                          maxVals         = list(),
                          fixedFit        = list(),
                          fitInitialGrid  = TRUE,
                          fitInitialGridN = 10,       # reduce if grid search 3/4+ parameters
                          fixedGrid       = list(),   # default only initial tau search
                          nCAF            = 5,
                          nDelta          = 19,
                          pDelta          = vector(),
                          tDelta          = 1,
                          costFunction    = "RMSE",
                          spDist          = 1,
                          drDist          = 0,
                          drShape         = 3,
                          drLim           = c(0.1, 0.7),
                          rtMax           = 5000,
                          subjects        = c(),
                          printInputArgs  = TRUE,
                          printResults    = FALSE,
                          optimxControl   = list()) {

  if (length(subjects) == 0) {
    subjects <- unique(resOb$summarySubject$Subject)  # fit all individual subjects in data
  }

  dmcfit <- vector("list", max(subjects))
  for (subject in subjects) {

    if (is.null(resOb$data)) {
      resObSubject <- list(
        deltaAgg = resOb$deltaSubject[resOb$deltaSubject$Subject == subject, ],
        cafAgg = resOb$cafSubject[resOb$cafSubject$Subject == subject, ]
      )
    } else {
      resObSubject <- list(
        data = resOb$data[resOb$data$Subject == subject, ],
        deltaAgg = resOb$deltaSubject[resOb$deltaSubject$Subject == subject, ],
        cafAgg = resOb$cafSubject[resOb$cafSubject$Subject == subject, ])
    }

    dmcfit[[subject]] <- dmcFit(resObSubject,
                                nTrl            = nTrl,
                                startVals       = startVals,
                                minVals         = minVals,
                                maxVals         = maxVals,
                                fixedFit        = fixedFit,
                                fitInitialGrid  = fitInitialGrid,
                                fitInitialGridN = fitInitialGridN, # reduce if grid search 3/4+ parameters
                                fixedGrid       = fixedGrid,       # only fit tau
                                nCAF            = nCAF,
                                nDelta          = nDelta,
                                pDelta          = pDelta,
                                tDelta          = tDelta,
                                costFunction    = costFunction,
                                spDist          = spDist,
                                drDist          = drDist,
                                drShape         = drShape,
                                drLim           = drLim,
                                rtMax           = rtMax,
                                printInputArgs  = printInputArgs,
                                printResults    = printResults,
                                optimxControl   = optimxControl)

  }

  class(dmcfit) <- "dmcfit"

  return(dmcfit)

}


#' @title dmcFitSubjectDE: Fit DMC to aggregated data using R-package DEoptim.
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions.
#'
#' @param resOb Observed data (see flankerData, simonData for data format)
#' @param nTrl Number of trials to use within dmcSim
#' @param minVals Minimum values for the to-be estimated parameters (see dmcFitDE)
#' @param maxVals Maximum values for the to-be estimated parameters (see dmcFitDE)
#' @param fixedFit Fix parameter to starting value (see dmcFitDE)
#' @param nCAF Number of CAF bins.
#' @param nDelta Number of delta bins.
#' @param pDelta Alternative to nDelta by directly specifying required percentile values
#' @param tDelta Type of delta calculation (1 = percentile, 2 = tile average)
#' @param costFunction Cost function to minimise root mean square error ("RMSE": default) or
#' squared percentage error ("SPE")
#' @param spDist starting point distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param drDist drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape drift rate (dr) shape parameter
#' @param drLim drift rate (dr) range
#' @param rtMax limit on simulated RT (decision + non-decisional component)
#' @param subjects NULL (aggregated data across all subjects) or integer for subject number
#' @param deControl Additional control parameters passes to DEoptim (see DEoptim.control)
#'
#' @return dmcfit_subject List of dmcfit per subject fitted (see dmcFitDM)
#'
#' @examples
#' \donttest{
#' # Example 1: Flanker data from Ulrich et al. (2015)
#' fit <- dmcFitSubjectDE(flankerData, nTrl = 1000, subjects = c(1, 2))
#' plot(fit, flankerData, subject = 1)
#' plot(fit, flankerData, subject = 2)
#' summary(fit)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFitSubjectDE(simonData, nTrl = 1000, subject = c(1, 2))
#' plot(fit, simonData, subject = 1)
#' plot(fit, simonData, subject = 2)
#' summary(fit)
#' }
#'
#' @export
dmcFitSubjectDE <- function(resOb,
                            nTrl         = 100000,
                            minVals      = list(),
                            maxVals      = list(),
                            fixedFit     = list(),
                            nCAF         = 5,
                            nDelta       = 19,
                            pDelta       = vector(),
                            tDelta       = 1,
                            costFunction = "RMSE",
                            spDist       = 1,
                            drDist       = 0,
                            drShape      = 3,
                            drLim        = c(0.1, 0.7),
                            rtMax        = 5000,
                            subjects     = c(),
                            deControl    = list()) {

  if (length(subjects) == 0) {
    subjects <- unique(resOb$summarySubject$Subject)  # fit all individual subjects in data
  }

  dmcfit <- vector("list", max(subjects))
  for (subject in subjects) {

    resObSubject <- list(deltaAgg = resOb$deltaSubject[resOb$deltaSubject$Subject == subject, ],
                         cafAgg = resOb$cafSubject[resOb$cafSubject$Subject == subject, ])

    dmcfit[[subject]] <- dmcFitDE(resObSubject,
                                  nTrl         = nTrl,
                                  minVals      = minVals,
                                  maxVals      = maxVals,
                                  fixedFit     = fixedFit,
                                  nCAF         = nCAF,
                                  nDelta       = nDelta,
                                  pDelta       = pDelta,
                                  tDelta       = tDelta,
                                  costFunction = costFunction,
                                  spDist       = spDist,
                                  rtMax        = rtMax,
                                  deControl    = deControl)

  }

  class(dmcfit) <- "dmcfit"

  return(dmcfit)

}




#' @title mean.dmcfit: Return mean simulation results from dmcFitSubject
#'
#' @description Aggregated simulation results from dmcFitSubject.
#'
#' @param x Output from dmcFitSubject
#' @param ... pars
#'
#' @return dmcfit
#'
#' The function returns a list with the relevant aggregated results dmcFitSubject. The list
#' is accessed with obj$name and so on with the the following:
#' \item{obj$means}{means}
#' \item{obj$delta}{delta}
#' \item{obj$caf}{caf}
#' \item{obj$prms}{par}
#'
#' @examples
#' \donttest{
#' # Example 1: Fit individual data then aggregate
#' fitSubjects <- dmcFitSubject(flankerData, nTrl = 1000, subjects = c(1, 2))
#' plot(fitSubjects, flankerData, subject = 1)
#' summary(fitSubjects)
#' fitAgg <- mean(fitSubjects)
#' plot(fitAgg, flankerData)
#' }
#'
#' @export
mean.dmcfit <- function(x, ...) {

  if ("sim" %in% names(x)) {  # aggregated fit
    stop("No individual data to aggregate!")
  }

  mergeLists <- function(lists, name) {
    return(lapply(c(name), function(x) do.call(rbind, lapply(lists, `[[`, x)))[[1]])
  }

  meanfit <- list()

  # summary
  meanfit$summary <- mergeLists(x, "summary") %>%
    dplyr::group_by(Comp) %>%
    dplyr::summarise(rtCor   = mean(rtCor),
                     sdCor   = mean(sdRtCor),
                     perErr  = mean(perErr),
                     rtErr   = mean(rtErr),
                     sdRtErr = mean(sdRtErr),
                     perSlow = mean(perSlow),
                     .groups = "drop")

  # delta
  meanfit$delta <- mergeLists(x, "delta") %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarise(meanComp   = mean(meanComp),
                     meanIncomp = mean(meanIncomp),
                     meanBin    = mean(meanBin),
                     meanEffect = mean(meanEffect),
                     .groups    = "drop")

  # caf
  meanfit$caf <- mergeLists(x, "caf") %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarise(accPerComp   = mean(accPerComp),
                     accPerIncomp = mean(accPerIncomp),
                     meanEffect   = mean(meanEffect),
                     .groups = "drop")

  # par
  meanfit$par <- as.data.frame(mergeLists(x, "par")) %>%
    dplyr::summarise(amp     = mean(unlist(amp)),
                     tau     = mean(unlist(tau)),
                     drc     = mean(unlist(drc)),
                     bnds    = mean(unlist(bnds)),
                     resMean = mean(unlist(resMean)),
                     resSD   = mean(unlist(resSD)),
                     aaShape = mean(unlist(aaShape)),
                     spShape = mean(unlist(spShape)),
                     spBias  = mean(unlist(spBias)),
                     sigm    = mean(unlist(sigm)),
                     cost    = mean(unlist(cost)),
                     .groups = "drop")

  class(meanfit) <- c("dmcfit")

  return(meanfit)

}

# function to minimise
minimizeCostValue <- function(x,
                              costFunction,
                              prms,
                              fixedFit,
                              resOb,
                              nTrl,
                              nDelta,
                              pDelta,
                              tDelta,
                              nCAF,
                              spDist,
                              drDist,
                              drShape,
                              drLim,
                              rtMax,
                              minVals,
                              maxVals,
                              printInputArgs,
                              printResults) {

  prms[!as.logical(fixedFit)] <- x

  # implement bounds
  prms <- as.list(pmax(unlist(prms), unlist(minVals)))
  prms <- as.list(pmin(unlist(prms), unlist(maxVals)))

  resTh <- dmcSim(amp = prms$amp, tau = prms$tau, drc = prms$drc, bnds = prms$bnds,
                  resMean = prms$resMean, resSD = prms$resSD, aaShape = prms$aaShape,
                  spDist = spDist, spShape = prms$spShape, spBias = prms$spBias, sigm = prms$sigm, spLim = c(-prms$bnds, prms$bnds),
                  drDist = drDist, drShape = drShape, drLim = drLim,
                  rtMax = rtMax,  nTrl = nTrl, nDelta = nDelta, pDelta = pDelta, tDelta = tDelta, nCAF = nCAF,
                  printInputArgs = printInputArgs, printResults = printResults)

  cost <- costFunction(resTh, resOb)
  cat(" | cost:", formatC(cost, 3, format = "f"))
  return(cost)

}

#' @title calculateCostValueRMSE: Calculate RMSE from RT and error data
#'
#' @description Calculate cost value (fit) from combination of RT and error rate.
#'
#' @param resTh list containing caf values for comp/incomp conditions (nbins * 4 columns) and
#' delta values for comp/incomp conditions (nbins * 5 columns). See output from dmcSim (.$caf).
#' @param resOb list containing caf values for comp/incomp conditions (n * 4 columns) and
#' delta values for comp/incomp conditions (nbins * 5 columns). See output from dmcSim (.$delta).
#'
#' @return cost value (RMSE)
#'
#' @examples
#' # Example 1:
#' resTh <- dmcSim()
#' resOb <- dmcSim()
#' cost  <- calculateCostValueRMSE(resTh, resOb)
#'
#' # Example 2:
#' resTh <- dmcSim()
#' resOb <- dmcSim(tau = 150)
#' cost  <- calculateCostValueRMSE(resTh, resOb)
#'
#' @export
calculateCostValueRMSE <- function(resTh, resOb) {

  n_rt  <- nrow(resTh$delta) * 2
  n_err <- nrow(resTh$caf) * 2

  costCAF   <- sqrt((1 / n_err) * sum((resTh$caf[c("accPerComp", "accPerIncomp")] - resOb$caf[c("accPerComp", "accPerIncomp")])**2))
  costRT    <- sqrt((1 / n_rt)  * sum((resTh$delta[c("meanComp", "meanIncomp")] - resOb$delta[c("meanComp", "meanIncomp")])**2))
  weightRT  <- n_rt / (n_rt + n_err)
  weightCAF <- (1 - weightRT) * 1500

  costValue <- (weightCAF * costCAF) + (weightRT * costRT)

  return(costValue)

}

#' @title calculateCostValueSPE: Calculate squared percentage error (SPE) from RT and error data
#'
#' @description Calculate cost value (fit) from combination of RT and error rate.
#'
#' @param resTh list containing caf values for comp/incomp conditions (nbins * 4 columns) and
#' delta values for comp/incomp conditions (nbins * 5 columns). See output from dmcSim (.$caf).
#' @param resOb list containing caf values for comp/incomp conditions (n * 4 columns) and
#' delta values for comp/incomp conditions (nbins * 5 columns). See output from dmcSim (.$delta).
#'
#' @return cost value (SPE)
#'
#' @examples
#' # Example 1:
#' resTh <- dmcSim()
#' resOb <- dmcSim()
#' cost  <- calculateCostValueSPE(resTh, resOb)
#'
#' # Example 2:
#' resTh <- dmcSim()
#' resOb <- dmcSim(tau = 150)
#' cost  <- calculateCostValueSPE(resTh, resOb)
#'
#' @export
calculateCostValueSPE <- function(resTh, resOb) {

  costCAF <- sum(((resOb$caf[, c("accPerComp", "accPerIncomp")]  -
      resTh$caf[, c("accPerComp", "accPerIncomp")]) / resOb$caf[, c("accPerComp", "accPerIncomp")])**2)

  costRT <- sum(((resOb$delta[, c("meanComp", "meanIncomp", "meanBin")] -
      resTh$delta[,  c("meanComp", "meanIncomp", "meanBin")]) / resOb$delta[, c("meanComp", "meanIncomp", "meanBin")])**2)

  costValue <- costRT + costCAF

  return(costValue)

}


#' @title calculateCostValueGS: Calculate likelihood-ratio chi-square statistic (GS) statistic from reaction times
#' for both correct and incorrect trials
#'
#' @description Calculate cost value (fit) from correct and incorrect RT data.
#'
#' @param resTh list containing simulation $sim values (output from dmcSim) for rts_comp, rts_incomp,
#' errs_comp, errs_incomp
#' @param resOb list containing raw observed data (see dmcObservedData with keepRaw = TRUE
#'
#' @return cost value (GS)
#'
#' @examples
#' # Example 1:
#' resTh <- dmcSim()
#' resOb <- flankerData
#' cost  <- calculateCostValueGS(resTh, flankerData)
#'
#' @export
calculateCostValueGS <- function(resTh, resOb, probs=c(0.1, 0.3, 0.5, 0.7, 0.9)) {

  nComp         <- nrow(resOb$data[resOb$data$Comp == "comp", ])
  gsCompCorrect <- gs(resTh$sim$rts_comp,  resOb$data$RT[resOb$data$Comp == "comp" & resOb$data$Error == 0], probs = probs)
  gsCompError   <- gs(resTh$sim$errs_comp, resOb$data$RT[resOb$data$Comp == "comp" & resOb$data$Error == 1], probs = probs)
  gsComp        <- nComp * (sum(gsCompCorrect) + sum(gsCompError))

  nIncomp         <- nrow(resOb$data[resOb$data$Comp == "incomp", ])
  gsIncompCorrect <- gs(resTh$sim$rts_incomp,  resOb$data$RT[resOb$data$Comp == "incomp" & resOb$data$Error == 0], probs = probs)
  gsIncompError   <- gs(resTh$sim$errs_incomp, resOb$data$RT[resOb$data$Comp == "incomp" & resOb$data$Error == 1], probs = probs)
  gsIncomp        <- nIncomp * (sum(gsIncompCorrect) + sum(gsIncompError))

  return(2*(gsComp + gsIncomp))

}

gs <- function(th, ob, probs=c(0.1, 0.3, 0.5, 0.7, 0.9)) {
  probs   <- c(0, probs, 1)
  pPred   <- diff(probs)
  qValues <- quantile(th, probs)
  nBin    <- table(.bincode(ob, qValues))
  pBin    <- nBin / sum(nBin)  # sum(pBin) = 1
  if (length(pBin) == length(pPred)) {
    out <- pBin * log(pBin/pPred)
  } else { # just use median if too few errors
    probs   <- c(0, 0.5, 1)
    pPred   <- diff(probs)
    qValues <- quantile(th, probs)
    nBin    <- table(.bincode(ob, qValues))
    pBin    <- nBin / sum(nBin)  # sum(pBin) = 1
    if (length(pBin) == length(pPred)) {
      out <- pBin * log(pBin/pPred)
    } else {
      out <- 0
    }
  }
  return(out)
}

