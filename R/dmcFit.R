#' @title dmcFit
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error ("RMSE") between a weighted combination
#' of the CAF and CDF functions using optim (Nelder-Mead). Alternative cost functions
#' include squared percentage error ("SPE"), and g-squared statistic ("GS").
#'
#' @param resOb Observed data (see flankerData and simonTask for data format) and the function dmcObservedData to create
#' the required input from either an R data frame or external *.txt/*.csv files
#' @param nTrl Number of trials to use within dmcSim.
#' @param startVals Starting values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., startVals = list(amp = 20, tau = 200,
#' drc = 0.5, bnds = 75, resMean = 300, resSD = 30, aaShape = 2, spShape = 3, spBias = 0, sigm = 4, bndsRate=0, bndsSaturation=0)).
#' @param minVals Minimum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., minVals = list(amp = 0, tau = 5, drc = 0.1,
#' bnds = 20, bndsRate=0, bndsSaturation=0, resMean = 200, resSD = 5, aaShape = 1, spShape = 2, spBias = -20, sigm = 1)).
#' @param maxVals Maximum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., maxVals = list(amp = 40, tau = 300, drc = 1.0,
#' bnds = 150, bndsRate=1, bndsSaturation=500, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 20, sigm = 10))
#' @param fixedFit Fix parameter to starting value. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., fixedFit = list(amp = FALSE, tau = FALSE, drc = FALSE,
#' bnds = FALSE, bndsRate=TRUE, bndsSaturation=TRUE, resMean = FALSE, resSD = FALSE, aaShape = FALSE, spShape = FALSE, spBias = TRUE, sigm = TRUE))
#' NB. Value if fixed at startVals.
#' @param freeCombined If fitting 2+ datasets at once, which parameters are allowed to vary between both
#' fits (default = all parameters fixed between the two fits e.g. parameter = FALSE).
#' This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., freeCombined = list(amp = FALSE,
#' tau = FALSE, drc = FALSE, bnds = FALSE, bndsRate=FALSE, bndsSaturation=FALSE, resMean = FALSE, resSD = FALSE, aaShape = FALSE, spShape = FALSE, spBias = FALSE, sigm = FALSE))
#' @param fitInitialGrid TRUE/FALSE
#' @param fitInitialGridN 10 linear steps between parameters min/max values (reduce if searching more than ~2/3 initial parameters)
#' @param fixedGrid Fix parameter for initial grid search. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., fixedGrid = list(amp = TRUE, tau = FALSE, drc = TRUE,
#' bnds = TRUE, bndsRate=TRUE, bndsSaturation=TRUE, resMean = TRUE, resSD = TRUE, aaShape = TRUE, spShape = TRUE, spBias = TRUE, sigm = TRUE)). As a default, the initial gridsearch
#' only searches the tau space.
#' @param nCAF The number of CAF bins.
#' @param nDelta The number of delta bins.
#' @param pDelta An alternative option to nDelta (tDelta = 1 only) by directly specifying required percentile values (vector of values 0-100)
#' @param tDelta The type of delta calculation (1=direct percentiles points, 2=percentile bounds (tile) averaging)
#' @param deltaErrors TRUE/FALSE Calculate delta bins for error trials
#' @param spDist The starting point (sp) distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param drOnset The starting point of controlled drift rate (i.e., "target" information) relative to automatic ("distractor" incormation) (> 0 ms)
#' @param drDist The drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape The drift rate (dr) shape parameter
#' @param drLim The drift rate (dr) range
#' @param rtMax The limit on simulated RT (decision + non-decisional components)
#' @param costFunction The cost function to minimise: root mean square error ("RMSE": default),
#' squared percentage error ("SPE"), or likelihood-ratio chi-square statistic ("GS")
#' @param printInputArgs TRUE (default) /FALSE
#' @param printResults TRUE/FALSE (default)
#' @param optimControl Additional control parameters passed to optim (see optim details section)
#' @param numCores Number of cores to use
#' @param setSeed TRUE/FALSE (default = TRUE) Lock the random seed for deterministic simulations.
#' @param seedValue (default = 1234) The seed value used if setSeed is TRUE.
#'
#' @return dmcfit returns an object of class "dmcfit" with the following components:
#'
#' \item{sim}{Individual trial data points (RTs for all trial types e.g., correct/error trials) and activation vectors from the simulation}
#' \item{summary}{Condition means for reaction time and error rate}
#' \item{caf}{Conditional Accuracy Function (CAF) data per bin}
#' \item{delta}{DataFrame with distributional delta analysis data correct trials (Bin, meanComp, meanIncomp, meanBin, meanEffect)}
#' \item{delta_errs}{DataFrame with distributional delta analysis data incorrect trials (Bin, meanComp, meanIncomp, meanBin, meanEffect)}
#' \item{par}{The fitted model parameters + final cost value of the fit}
#'
#' @examples
#' \donttest{
#' # Code below can exceed CRAN check time limit, hence donttest
#' # Example 1: Flanker data from Ulrich et al. (2015)
#' fit <- dmcFit(flankerData) # only initial search tau
#' plot(fit, flankerData)
#' summary(fit)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFit(simonData) # only initial search tau
#' plot(fit, simonData)
#' summary(fit)
#'
#' # Example 3: Flanker data from Ulrich et al. (2015) with non-default
#' # start vals and some fixed values
#' fit <- dmcFit(flankerData,
#'   startVals = list(drc = 0.6, aaShape = 2.5),
#'   fixedFit = list(drc = TRUE, aaShape = TRUE)
#' )
#'
#' # Example 4: Simulated Data (+ve going delta function)
#' dat <- createDF(nSubjects = 20, nTrl = 500, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'   RT = list(
#'     "Comp_comp" = c(510, 100, 100),
#'     "Comp_incomp" = c(540, 130, 85)
#'   ),
#'   Error = list(
#'     "Comp_comp" = c(4, 3, 2, 1, 1),
#'     "Comp_incomp" = c(20, 4, 3, 1, 1)
#'   )
#' )
#' datOb <- dmcObservedData(dat, columns = c("Subject", "Comp", "RT", "Error"))
#' plot(datOb)
#' fit <- dmcFit(datOb, nTrl = 5000)
#' plot(fit, datOb)
#' summary(fit)
#'
#' # Example 5: Fitting 2+ datasets within all common parameters values
#' fit <- dmcFit(list(flankerData, simonData), nTrl=1000)
#' plot(fit[[1]], flankerData)
#' plot(fit[[2]], simonData)
#' summary(fit)
#'
#' # Example 6: Fitting 2+ datasets within some parameters values varying
#' fit <- dmcFit(list(flankerData, simonData), freeCombined=list(amp=TRUE, tau=TRUE), nTrl=1000)
#' summary(fit) # NB. amp/tau values different, other parameter values equal
#' }
#'
#'
#' @export
dmcFit <- function(resOb,
                   nTrl            = 100000,
                   startVals       = list(),
                   minVals         = list(),
                   maxVals         = list(),
                   fixedFit        = list(),
                   freeCombined    = list(),
                   fitInitialGrid  = TRUE,
                   fitInitialGridN = 10, # reduce if grid search 3/4+ parameters
                   fixedGrid       = list(), # default only initial search tau
                   nCAF            = 5,
                   nDelta          = 19,
                   pDelta          = vector(),
                   tDelta          = 1,
                   deltaErrors     = FALSE,
                   spDist          = 1,
                   drOnset         = 0,
                   drDist          = 0,
                   drShape         = 3,
                   drLim           = c(0.1, 0.7),
                   rtMax           = 5000,
                   costFunction    = "RMSE",
                   printInputArgs  = TRUE,
                   printResults    = FALSE,
                   optimControl    = list(),
                   numCores        = 2,
                   setSeed         = TRUE,
                   seedValue       = 1234) {

  if (inherits(resOb, "dmcob")) {
    resOb <- list(resOb)
  }

  # default parameter space
  defaultStartVals    <- list(amp = 20, tau = 200, drc = 0.5, bnds = 75,  bndsRate=0, bndsSaturation=0,   resMean = 300, resSD = 30,  aaShape = 2, spShape = 3, spBias = 0,   sigm = 4)
  defaultMinVals      <- list(amp = 0,  tau = 5,   drc = 0.1, bnds = 20,  bndsRate=0, bndsSaturation=0,   resMean = 200, resSD = 5,   aaShape = 1, spShape = 2, spBias = -20, sigm = 1)
  defaultMaxVals      <- list(amp = 40, tau = 300, drc = 1.0, bnds = 150, bndsRate=1, bndsSaturation=500, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 20,  sigm = 10)
  defaultFixedFit     <- list(amp = FALSE,  tau = FALSE,   drc = FALSE,   bnds = FALSE,   bndsRate=TRUE, bndsSaturation=TRUE,   resMean = FALSE,   resSD = FALSE,   aaShape = FALSE, spShape = FALSE, spBias = TRUE,   sigm = TRUE)
  defaultFixedGrid    <- list(amp = TRUE,  tau = FALSE,   drc = TRUE,   bnds = TRUE,   bndsRate=TRUE, bndsSaturation=TRUE,   resMean = TRUE,   resSD = TRUE,   aaShape = TRUE, spShape = TRUE, spBias = TRUE,   sigm = TRUE)
  defaultFreeCombined <- list(amp = FALSE,  tau = FALSE,   drc = FALSE,   bnds = FALSE,   bndsRate=FALSE, bndsSaturation=FALSE,   resMean = FALSE,   resSD = FALSE,   aaShape = FALSE, spShape = FALSE, spBias = FALSE,   sigm = FALSE)

  startVals <- modifyList(defaultStartVals, startVals)
  # startVals <- lapply(startVals, function(x) ifelse(x == 0, .Machine$double.xmin, x))
  if (!fitInitialGrid) {
    startVals <- unlist(startVals)
  }

  minVals      <- modifyList(defaultMinVals, minVals)
  maxVals      <- modifyList(defaultMaxVals, maxVals)
  fixedFit     <- modifyList(defaultFixedFit, fixedFit)
  fixedGrid    <- modifyList(defaultFixedGrid, fixedGrid)
  freeCombined <- modifyList(defaultFreeCombined, freeCombined)

  # parScale <- unlist(startVals) / min(unlist(startVals) + 1)
  # hard-code parscale to approx above, but avoid 0's?
  parScale <- list(amp = 20, tau = 200, drc = 0.5, bnds = 75, bndsRate=0.5, bndsSaturation=200, resMean = 300, resSD = 30, aaShape = 2, spShape = 3, spBias = 20, sigm = 4)

  prms <- replicate(length(resOb), startVals, simplify = FALSE)

  # default optim control parameters
  defaultOptimControl <- list(parscale = c(parScale[!as.logical(fixedFit)], rep(parScale[as.logical(freeCombined)], length(resOb)-1)), maxit = 500, reltol = 1e-4)
  optimControl <- modifyList(defaultOptimControl, optimControl)

  # change nDelta to pDelta if pDelta not empty
  if (length(pDelta) != 0) {
    nDelta <- pDelta
    tDelta <- 1
  }

  # check observed data contains correct number of delta/CAF bins
  for (i in 1:length(resOb)) {
    if (length(nDelta) == 1) {
      if (nrow(resOb[[i]]$delta) != nDelta) {
        stop(paste0("Number of delta bins in observed data set ", i, " and nDelta bins are not equal!"))
      }
    } else if (length(nDelta) > 1) {
      if (nrow(resOb[[i]]$delta) != length(nDelta)) {
        stop(paste0("Number of delta bins in observed data set ", i, " and nDelta bins are not equal!"))
      }
    }
    if (nrow(resOb[[i]]$caf) != nCAF) {
      stop(paste0("Number of CAF bins in observed data set ", i, " and nCAF bins are not equal!"))
    }
  }

  # which cost function?
  calculateCostValue <- costValueFunction(costFunction)

  if (is.character(costFunction)) {
    if (tolower(costFunction) %in% c("cs", "gs")) {
      # add additional probabilities to observed data required for cs/gs cost functions
      for (i in 1:length(resOb)) {
        resOb[[i]] <- calculateBinProbabilities(resOb[[i]])
      }
    }
  }

  # fit procedure initial grid
  if (fitInitialGrid) {
    minValsGrid <- minVals
    maxValsGrid <- maxVals
    startValsGrid <- startVals

    # which parameters to search within grid
    if (TRUE %in% fixedGrid) {
      minValsGrid[fixedGrid == TRUE] <- startValsGrid[fixedGrid == TRUE]
      maxValsGrid[fixedGrid == TRUE] <- startValsGrid[fixedGrid == TRUE]
    }

    startValsGrid <- Map(seq, minValsGrid, maxValsGrid, length.out = fitInitialGridN)
    startValsGrid <- dplyr::distinct(expand.grid(Map(unique, startValsGrid)))
    message("Searching initial parameter gridspace: N = ", nrow(startValsGrid), ", with ", nTrl, " trials per simulation.")

    # CRAN maintainer personal communication: THIS IS NOT THE WAY TO DO IT!
    # CRAN submissions can only use maximum of two cores unless user requests more!
    # R check limits number of cores to 2 (https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions)
    # chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    # if (nzchar(chk) && (chk == "true")) {
    #   num_cores <- 2L
    # } else {
    #   num_cores <- parallel::detectCores() / 2
    # }

    # Older code (pre June 2020) used doSNOW progress bar
    # R CMD check --as-cran -->  doSNOW warning “superseded packages”
    # Use pbapply for progress bar
    # https://stackoverflow.com/questions/58473626/r-doparallel-progress-bar-to-monitor-finished-jobs

    pCostValue <- function(i) {
      resTh <- dmcSim(
        amp = startValsGrid$amp[i], tau = startValsGrid$tau[i], drc = startValsGrid$drc[i], bnds = startValsGrid$bnds[i],
        resMean = startValsGrid$resMean[i], resSD = startValsGrid$resSD[i], aaShape = startValsGrid$aaShape[i],
        spShape = startValsGrid$spShape[i], spBias = startValsGrid$spBias[i], sigm = startValsGrid$sigm[i],
        spLim = c(-startValsGrid$bnds[i], startValsGrid$bnds[i]), spDist = spDist, bndsRate = startValsGrid$bndsRate[i],
        bndsSaturation = startValsGrid$bndsSaturation[i], drOnset = drOnset, drDist = drDist, drShape = drShape, drLim = drLim,
        rtMax = rtMax, nTrl = nTrl, nDelta = nDelta, pDelta = pDelta, tDelta = tDelta, deltaErrors = deltaErrors, nCAF = nCAF,
        printInputArgs = TRUE, printResults = FALSE
      )
      cost <- 0
      for (i in 1:length(resOb)) {
        cost <- cost + calculateCostValue(resTh, resOb[[i]])
      }
      return(cost)
    }

    cl <- parallel::makeCluster(numCores)
    parallel::clusterEvalQ(cl, require("DMCfun"))
    on.exit({ try(parallel::stopCluster(cl), silent = TRUE) }, add = TRUE)
    invisible(parallel::clusterExport(cl = cl, varlist = c("dmcSim", "calculateCostValue", "calculateCostValueRMSE_cpp", "calculateCostValueSPE_cpp", "calculateCostValueCS_cpp", "calculateCostValueGS_cpp"), envir = environment()))

    # calculate initial cost values across grid starting values and find min
    costValue <- pbapply::pblapply(cl = cl, X = 1:nrow(startValsGrid), FUN = pCostValue)
    startVals <- startValsGrid[which.min(costValue), ]

    parallel::stopCluster(cl)
  }

  # optimize
  fit <- optim(
    par = as.numeric(c(startVals[!as.logical(fixedFit)], rep(startVals[as.logical(freeCombined)], length(resOb)-1))),
    fn = minimizeCostValue,
    costFunction = calculateCostValue, prms = prms, fixedFit = fixedFit, freeCombined = freeCombined, resOb = resOb,
    nTrl = nTrl, nDelta = nDelta, pDelta = pDelta, tDelta = tDelta, deltaErrors = deltaErrors, nCAF = nCAF,
    spDist = spDist, drOnset = drOnset, drDist = drDist,
    drShape = drShape, drLim = drLim, rtMax = rtMax, minVals = minVals, maxVals = maxVals,
    printInputArgs = printInputArgs, printResults = printResults,
    setSeed = setSeed, seedValue = seedValue,
    method = "Nelder-Mead", control = optimControl
  )

  # number of fitted parameters
  nFreeParametersSingle <- sum(as.logical(fixedFit) == FALSE)
  nFreeParametersTotal  <- sum(as.logical(fixedFit) == FALSE) + (sum(as.logical(freeCombined == TRUE)) * (length(resOb)-1))

  for (i in 1:length(resOb)) {
    prms[[i]][!as.logical(fixedFit)] <- fit$par[c(1:nFreeParametersSingle)]
  }
  if (length(resOb) >= 2) {
    startPos      <- nFreeParametersSingle + 1
    nFreePerResOb <- (nFreeParametersTotal - nFreeParametersSingle) / (length(resOb)-1)
    for (i in 2:length(resOb)) {
      prms[[i]][as.logical(freeCombined)] <- fit$par[startPos:(startPos+(nFreePerResOb-1))]
      startPos <- startPos + nFreePerResOb
    }
  }

  # bounds check
  for (i in 1:length(prms)) {
    prms[[i]] <- pmax(unlist(prms[[i]]), unlist(minVals))
    prms[[i]] <- pmin(unlist(prms[[i]]), unlist(maxVals))
  }
  for (i in 1:length(resOb)) {
    tmp_minVals <- unlist(minVals[!as.logical(fixedFit)])
    tmp_maxVals <- unlist(maxVals[!as.logical(fixedFit)])
    tmp_prms <- unlist(prms[[i]][!as.logical(fixedFit)])
    if (any(tmp_prms == tmp_minVals) || any(tmp_prms == tmp_maxVals)) {
      warning(paste0("Parameter estimates at minVals/maxVals bounds for data set ", i, "!"))
    }
  }
  for (i in 1:length(resOb)) {
    prms[[i]] <- as.list(prms[[i]])
  }

  dmcfit <- list()
  for (i in 1:length(resOb)) {
    dmcfit[[i]] <- dmcSim(
      amp = prms[[i]]$amp, tau = prms[[i]]$tau, drc = prms[[i]]$drc, bnds = prms[[i]]$bnds,
      resMean = prms[[i]]$resMean, resSD = prms[[i]]$resSD, aaShape = prms[[i]]$aaShape,
      spDist = spDist, spBias = prms[[i]]$spBias, spShape = prms[[i]]$spShape, spLim = c(-prms[[i]]$bnds, prms[[i]]$bnds),
      sigm = prms[[i]]$sigm, bndsRate = prms[[i]]$bndsRate, bndsSaturation = prms[[i]]$bndsSaturation, drOnset = drOnset,
      drDist = drDist, drShape = drShape, drLim = drLim, rtMax = rtMax, nTrl = nTrl, nDelta = nDelta, pDelta = pDelta,
      tDelta = tDelta, deltaErrors = deltaErrors, nCAF = nCAF, printResults = TRUE
    )
  }

  # fitted parameters
  for (i in 1:length(resOb)) {
    dmcfit[[i]]$nDataSets <- length(resOb)
    dmcfit[[i]]$par <- prms[[i]]
    dmcfit[[i]]$par["cost"] <- calculateCostValue(dmcfit[[i]], resOb[[i]])
  }

  if (!is.function(costFunction)) {
    if (tolower(costFunction) %in% c("cs", "gs")) {
      for (i in 1:length(dmcfit)) {
        dmcfit[[i]]$par["df"] <- (2 * (12 - 1)) - nFreeParametersTotal # 2 conditions with 6 bins - N fitted parameters
      }
    }
    if (tolower(costFunction) == "gs") {
      for (i in 1:length(dmcfit)) {
        dmcfit[[i]]$par["BIC"] <- dmcfit[[i]]$par$cost + (nFreeParametersTotal * log(sum(resOb[[i]]$data$outlier == 0)))
      }
    }
  }

  dmcfit <- lapply(dmcfit, `class<-`, value = "dmcfit")
  if (length(dmcfit) == 1) {
    return(dmcfit[[1]])
  } else {
    class(dmcfit) <- "dmcfits"
    return(dmcfit)
  }

}


#' @title dmcFitDE
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions using the R-package DEoptim. Alternative cost functions
#' include squared percentage error ("SPE"), and g-squared statistic ("GS").
#'
#' @param resOb Observed data (see flankerData and simonTask for data format)
#' @param nTrl The number of trials to use within dmcSim.
#' @param minVals Minimum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., minVals = list(amp = 10, tau = 5, drc = 0.1,
#' bnds = 20, bndsRate=0, bndsSaturation=0, resMean = 200, resSD = 5, aaShape = 1, spShape = 2, spBias = -20, sigm = 1)).
#' @param maxVals Maximum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., maxVals = list(amp = 40, tau = 300, drc = 1.0,
#' bnds = 150, bndsRate=1, bndsSaturation=500, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 20, sigm = 10))
#' @param fixedFit Fix parameter to starting value. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., fixedFit = list(amp = FALSE, tau = FALSE, drc = FALSE,
#' bnds = FALSE, bndsRate=TRUE, bndsSaturation=TRUE, resMean = FALSE, resSD = FALSE, aaShape = FALSE, spShape = FALSE, spBias = TRUE, sigm = TRUE))
#' NB. Value if fixed at startVals.
#' @param freeCombined If fitting 2+ datasets at once, which parameters are allowed to vary between both
#' fits (default = all parameters fixed between the two fits e.g. parameter = FALSE).
#' This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., freeCombined = list(amp = FALSE,
#' tau = FALSE, drc = FALSE, bnds = FALSE, bndsRate=FALSE, bndsSaturation=FALSE, resMean = FALSE, resSD = FALSE, aaShape = FALSE, spShape = FALSE, spBias = FALSE, sigm = FALSE))
#' @param nCAF The number of CAF bins.
#' @param nDelta The number of delta bins.
#' @param pDelta An alternative option to nDelta (tDelta = 1 only) by directly specifying required percentile values (vector of values 0-100)
#' @param tDelta The type of delta calculation (1=direct percentiles points, 2=percentile bounds (tile) averaging)
#' @param deltaErrors TRUE/FALSE Calculate delta bins for error trials
#' @param spDist The starting point distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param drOnset The starting point of controlled drift rate (i.e., "target" information) relative to automatic ("distractor" information) (> 0 ms)
#' @param drDist The drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape The drift rate (dr) shape parameter
#' @param drLim The drift rate (dr) range
#' @param rtMax The limit on simulated RT (decision + non-decisional components)
#' @param costFunction The cost function to minimise: root mean square error ("RMSE": default),
#' squared percentage error ("SPE"), or likelihood-ratio chi-square statistic ("GS")
#' @param deControl Additional control parameters passed to DEoptim (see DEoptim.control)
#' @param numCores Number of cores to use
#' @param setSeed TRUE/FALSE (default = TRUE) Lock the random seed for deterministic simulations.
#' @param seedValue (default = 1234) The seed value used if setSeed is TRUE.
#'
#' @return dmcfit returns an object of class "dmcfit" with the following components:
#'
#' \item{sim}{Individual trial data points (RTs for all trial types e.g., correct/error trials) and activation vectors from the simulation}
#' \item{summary}{Condition means for reaction time and error rate}
#' \item{caf}{Conditional Accuracy Function (CAF) data per bin}
#' \item{delta}{DataFrame with distributional delta analysis data correct trials (Bin, meanComp, meanIncomp, meanBin, meanEffect)}
#' \item{delta_errs}{Optional: DataFrame with distributional delta analysis data incorrect trials (Bin, meanComp, meanIncomp, meanBin, meanEffect)}
#' \item{par}{The fitted model parameters + final cost value of the fit}
#'
#' @examples
#' \donttest{
#' # The code below can exceed CRAN check time limit, hence donttest
#' # NB. The following code when using numCores = 2 (default) takes approx 20 minutes on
#' # a standard desktop, whilst when increasing the number of cores used, (numCores = 12),
#' # the code takes approx 5 minutes.
#'
#' # Example 1: Flanker data from Ulrich et al. (2015)
#' fit <- dmcFitDE(flankerData, nTrl = 1000);
#' plot(fit, flankerData)
#' summary(fit)
#'
#' # Example 2: Simon data from Ulrich et al. (2015)
#' fit <- dmcFitDE(simonData, nTrl = 5000, deControl = list(itermax=30))
#' plot(fit, simonData)
#' summary(fit)
#' }
#' @export
dmcFitDE <- function(resOb,
                     nTrl         = 100000,
                     minVals      = list(),
                     maxVals      = list(),
                     fixedFit     = list(),
                     freeCombined = list(),
                     nCAF         = 5,
                     nDelta       = 19,
                     pDelta       = vector(),
                     tDelta       = 1,
                     deltaErrors  = FALSE,
                     spDist       = 1,
                     drOnset      = 0,
                     drDist       = 0,
                     drShape      = 3,
                     drLim        = c(0.1, 0.7),
                     rtMax        = 5000,
                     costFunction = "RMSE",
                     deControl    = list(),
                     numCores     = 2,
                     setSeed      = TRUE,
                     seedValue    = 1234) {

  if (inherits(resOb, "dmcob")) {
    resOb <- list(resOb)
  }

  # default parameter space
  defaultMinVals      <- list(amp = 0,  tau = 5,   drc = 0.1, bnds = 20,  bndsRate=0, bndsSaturation=0, resMean = 200, resSD = 5,   aaShape = 1, spShape = 2, spBias = 0, sigm = 4)
  defaultMaxVals      <- list(amp = 40, tau = 300, drc = 1.0, bnds = 150, bndsRate=0, bndsSaturation=0, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 0, sigm = 4)
  defaultFixedFit     <- list(amp = FALSE,  tau = FALSE,   drc = FALSE,   bnds = FALSE,   bndsRate=TRUE, bndsSaturation=TRUE, resMean = FALSE,   resSD = FALSE,   aaShape = FALSE, spShape = FALSE, spBias = TRUE, sigm = TRUE)
  defaultFreeCombined <- list(amp = FALSE,  tau = FALSE,   drc = FALSE,   bnds = FALSE,   bndsRate=FALSE, bndsSaturation=FALSE, resMean = FALSE,   resSD = FALSE,   aaShape = FALSE, spShape = FALSE, spBias = FALSE, sigm = FALSE)

  minVals      <- modifyList(defaultMinVals, minVals)
  maxVals      <- modifyList(defaultMaxVals, maxVals)
  fixedFit     <- modifyList(defaultFixedFit, fixedFit)
  freeCombined <- modifyList(defaultFreeCombined, freeCombined)

  # NB will need to explicitly set min/max values for bndsRate/bndsSaturation/spBias if fitting!
  prms <- replicate(length(resOb), (unlist(minVals) + unlist(maxVals)) / 2, simplify = FALSE) # start in middle of min/max vals

  # change nDelta to pDelta if pDelta not empty
  if (length(pDelta) != 0) {
    nDelta <- pDelta
    tDelta <- 1
  }

  # check observed data contains correct number of delta/CAF bins
  for (i in 1:length(resOb)) {
    if (length(nDelta) == 1) {
      if (nrow(resOb[[i]]$delta) != nDelta) {
        stop(paste0("Number of delta bins in observed data set ", i, "and nDelta bins are not equal!"))
      }
    } else if (length(nDelta) > 1) {
      if (nrow(resOb[[i]]$delta) != length(nDelta)) {
        stop(paste0("Number of delta bins in observed data set ", i, "and nDelta bins are not equal!"))
      }
    }
    if (nrow(resOb[[i]]$caf) != nCAF) {
      stop(paste0("Number of CAF bins in observed data set ", i, "and nCAF bins are not equal!"))
    }
  }

  # which cost function?
  calculateCostValue <- costValueFunction(costFunction)

  if (is.character(costFunction)) {
    if (tolower(costFunction) %in% c("cs", "gs")) {
      # add additional probabilities to observed data required for cs/gs cost functions
      for (i in 1:length(resOb)) {
        resOb[[i]] <- calculateBinProbabilities(resOb[[i]])
      }
    }
  }

  # CRAN maintainer personal communication: THIS IS NOT THE WAY TO DO IT!
  # CRAN submissions can only use maximum of two cores unless user requests more!
  # R check limits number of cores to 2 (https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions)
  # chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  # if (nzchar(chk) && (chk == "true")) {
  #   num_cores <- 2L
  # } else {
  #   num_cores <- parallel::detectCores() / 2
  # }

  cl <- parallel::makeCluster(numCores)
    parallel::clusterEvalQ(cl, require("DMCfun"))
    on.exit({ try(parallel::stopCluster(cl), silent = TRUE) }, add = TRUE)
  invisible(parallel::clusterExport(cl = cl, varlist = c("dmcSim", "calculateCostValue", "calculateCostValueRMSE_cpp", "calculateCostValueSPE_cpp", "calculateCostValueCS_cpp", "calculateCostValueGS_cpp"), envir = environment()))

  nParameters <- length(c(unlist(minVals[!as.logical(fixedFit)]), rep(unlist(minVals[as.logical(freeCombined)]), length(resOb)-1)))
  defaultControl <- list(VTR = 0, strategy = 1, NP = max(50, 10 * nParameters), itermax = 200, reltol = 1e-4, steptol = 20, trace = 1, cluster = cl)
  deControl <- modifyList(defaultControl, deControl)

  # optimize
  fit <- DEoptim::DEoptim(
    fn             = minimizeCostValue,
    lower          = c(unlist(minVals[!as.logical(fixedFit)]), rep(unlist(minVals[as.logical(freeCombined)]), length(resOb)-1)),
    upper          = c(unlist(maxVals[!as.logical(fixedFit)]), rep(unlist(maxVals[as.logical(freeCombined)]), length(resOb)-1)),
    costFunction   = calculateCostValue,
    prms           = prms,
    fixedFit       = fixedFit,
    freeCombined   = freeCombined,
    minVals        = minVals,
    maxVals        = maxVals,
    resOb          = resOb,
    nTrl           = nTrl,
    nDelta         = nDelta,
    nCAF           = nCAF,
    pDelta         = pDelta,
    tDelta         = tDelta,
    deltaErrors    = deltaErrors,
    spDist         = spDist,
    drOnset        = drOnset,
    drDist         = drDist,
    drShape        = drShape,
    drLim          = drLim,
    rtMax          = rtMax,
    setSeed        = setSeed,
    seedValue      = seedValue,
    printInputArgs = FALSE,
    printResults   = FALSE,
    control        = deControl
  )

  parallel::stopCluster(cl)

  # number of fitted parameters
  nFreeParametersSingle <- sum(as.logical(fixedFit) == FALSE)
  nFreeParametersTotal  <- sum(as.logical(fixedFit) == FALSE) + (sum(as.logical(freeCombined == TRUE)) * (length(resOb)-1))

  for (i in 1:length(resOb)) {
    prms[[i]][!as.logical(fixedFit)] <- as.list(fit$optim$bestmem)[c(1:nFreeParametersSingle)]
  }
  if (length(resOb) >= 2) {
    startPos      <- nFreeParametersSingle + 1
    nFreePerResOb <- (nFreeParametersTotal - nFreeParametersSingle) / (length(resOb)-1)
    for (i in 2:length(resOb)) {
      prms[[i]][as.logical(freeCombined)] <- as.list(fit$optim$bestmem)[startPos:(startPos+(nFreePerResOb-1))]
      startPos <- startPos + nFreePerResOb
    }
  }

  # bounds check
  for (i in 1:length(prms)) {
    prms[[i]] <- pmax(unlist(prms[[i]]), unlist(minVals))
    prms[[i]] <- pmin(unlist(prms[[i]]), unlist(maxVals))
  }
  for (i in 1:length(resOb)) {
    tmp_minVals <- unlist(minVals[!as.logical(fixedFit)])
    tmp_maxVals <- unlist(maxVals[!as.logical(fixedFit)])
    tmp_prms <- unlist(prms[[i]][!as.logical(fixedFit)])
    if (any(tmp_prms == tmp_minVals) || any(tmp_prms == tmp_maxVals)) {
      warning(paste0("Parameter estimates at minVals/maxVals bounds for data set ", i, "!"))
    }
  }
  for (i in 1:length(resOb)) {
    prms[[i]] <- as.list(prms[[i]])
  }
  cat("\n")

  dmcfit <- list()
  for (i in 1:length(resOb)) {
    dmcfit[[i]] <- dmcSim(
      amp = prms[[i]]$amp, tau = prms[[i]]$tau, drc = prms[[i]]$drc, bnds = prms[[i]]$bnds,
      resMean = prms[[i]]$resMean, resSD = prms[[i]]$resSD, aaShape = prms[[i]]$aaShape,
      spDist = spDist, spBias = prms[[i]]$spBias, spShape = prms[[i]]$spShape, spLim = c(-prms[[i]]$bnds, prms[[i]]$bnds),
      sigm = prms[[i]]$sigm, drOnset = drOnset, bndsRate = prms[[i]]$bndsRate, bndsSaturation = prms[[i]]$bndsSaturation,
      drDist = drDist, drShape = drShape, drLim = drLim, rtMax = rtMax, nTrl = nTrl, nDelta = nDelta,
      pDelta = pDelta, tDelta = tDelta, deltaErrors = deltaErrors, nCAF = nCAF,
      printResults = TRUE
    )
  }

  dmcfit[[1]]$sim$errs_comp

  # fitted parameters
  for (i in 1:length(resOb)) {
    dmcfit[[i]]$nDataSets <- length(resOb)
    dmcfit[[i]]$par <- prms[[i]]
    dmcfit[[i]]$par["cost"] <- calculateCostValue(dmcfit[[i]], resOb[[i]])
  }

  if (!is.function(costFunction)) {
    if (tolower(costFunction) %in% c("cs", "gs")) {
      for (i in 1:length(dmcfit)) {
        dmcfit[[i]]$par["df"] <- (2 * (12 - 1)) - nFreeParametersTotal # 2 conditions with 6 bins - N fitted parameters
      }
    }
    if (tolower(costFunction) == "gs") {
      for (i in 1:length(dmcfit)) {
        dmcfit[[i]]$par["BIC"] <- dmcfit[[i]]$par$cost + (nFreeParametersTotal * log(sum(resOb[[i]]$data$outlier == 0)))
      }
    }
  }

  dmcfit <- lapply(dmcfit, `class<-`, value = "dmcfit")
  if (length(dmcfit) == 1) {
    return(dmcfit[[1]])
  } else {
    class(dmcfit) <- "dmcfits"
    return(dmcfit)
  }

  return(dmcfit)
}


#' @title dmcFitSubject
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error ("RMSE") between a weighted combination
#' of the CAF and CDF functions using optim (Nelder-Mead). Alternative cost functions
#' include squared percentage error ("SPE"), and g-squared statistic ("GS").
#'
#' @param resOb Observed data (see flankerData and simonTask for data format) and the function dmcObservedData to create
#' the required input from either an R data frame or external *.txt/*.csv files
#' @param nTrl Number of trials to use within dmcSim.
#' @param startVals Starting values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., startVals = list(amp = 20, tau = 200,
#' drc = 0.5, bnds = 75, resMean = 300, resSD = 30, aaShape = 2, spShape = 3, spBias = 0, sigm = 4, bndsRate=0, bndsSaturation=0)).
#' @param minVals Minimum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., minVals = list(amp = 0, tau = 5, drc = 0.1,
#' bnds = 20, bndsRate=0, bndsSaturation=0, resMean = 200, resSD = 5, aaShape = 1, spShape = 2, spBias = -20, sigm = 1)).
#' @param maxVals Maximum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., maxVals = list(amp = 40, tau = 300, drc = 1.0,
#' bnds = 150, bndsRate=1, bndsSaturation=500, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 20, sigm = 10))
#' @param fixedFit Fix parameter to starting value. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., fixedFit = list(amp = FALSE, tau = FALSE, drc = FALSE,
#' bnds = FALSE, bndsRate=TRUE, bndsSaturation=TRUE, resMean = FALSE, resSD = FALSE, aaShape = FALSE, spShape = FALSE, spBias = TRUE, sigm = TRUE))
#' NB. Value if fixed at startVals.
#' @param fitInitialGrid TRUE/FALSE
#' @param fitInitialGridN 10 linear steps between parameters min/max values (reduce if searching more than ~2/3 initial parameters)
#' @param fixedGrid Fix parameter for initial grid search. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., fixedGrid = list(amp = TRUE, tau = FALSE, drc = TRUE,
#' bnds = TRUE, bndsRate=TRUE, bndsSaturation=TRUE, resMean = TRUE, resSD = TRUE, aaShape = TRUE, spShape = TRUE, spBias = TRUE, sigm = TRUE)). As a default,
#' the initial gridsearch only searches the tau space.
#' @param freeCombined If fitting 2+ datasets at once, which parameters are allowed to vary between both
#' fits (default = all parameters fixed between the two fits e.g. parameter = FALSE).
#' This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., freeCombined = list(amp = FALSE,
#' tau = FALSE, drc = FALSE, bnds = FALSE, bndsRate=FALSE, bndsSaturation=FALSE, resMean = FALSE, resSD = FALSE, aaShape = FALSE, spShape = FALSE, spBias = FALSE, sigm = FALSE))
#' @param nCAF Number of CAF bins.
#' @param nDelta Number of delta bins.
#' @param pDelta An alternative option to nDelta (tDelta = 1 only) by directly specifying required percentile values (vector of values 0-100)
#' @param tDelta The type of delta calculation (1=direct percentiles points, 2=percentile bounds (tile) averaging)
#' @param deltaErrors TRUE/FALSE Calculate delta bins for error trials
#' @param spDist The starting point (sp) distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param drOnset The starting point of controlled drift rate (i.e., "target" information) relative to automatic ("distractor" incormation) (> 0 ms)
#' @param drDist The drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape The drift rate (dr) shape parameter
#' @param drLim The drift rate (dr) range
#' @param rtMax The limit on simulated RT (decision + non-decisional components)
#' @param costFunction The cost function to minimise: root mean square error ("RMSE": default),
#' squared percentage error ("SPE"), or likelihood-ratio chi-square statistic ("GS")
#' @param subjects NULL (aggregated data across all subjects) or integer for subject number
#' @param printInputArgs TRUE (default) /FALSE
#' @param printResults TRUE/FALSE (default)
#' @param optimControl Additional control parameters passed to optim (see optim details section)
#' @param numCores Number of cores to use
#' @param setSeed TRUE/FALSE (default = TRUE) Lock the random seed for deterministic simulations.
#' @param seedValue (default = 1234) The seed value used if setSeed is TRUE.
#'
#' @return dmcFitSubject returns a list of objects of class "dmcfit"
#'
#' @examples
#' \donttest{
#' # Code below can exceed CRAN check time limit, hence donttest
#' # Example 1: Flanker data from Ulrich et al. (2015)
#' fit <- dmcFitSubject(flankerData, nTrl = 1000, subjects = c(1, 2));
#' plot(fit, flankerData, subject = 1)
#' plot(fit, flankerData, subject = 2)
#' summary(fit)
#'
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
                          fitInitialGridN = 10, # reduce if grid search 3/4+ parameters
                          fixedGrid       = list(), # default only initial tau search
                          freeCombined    = list(),
                          nCAF            = 5,
                          nDelta          = 19,
                          pDelta          = vector(),
                          tDelta          = 1,
                          deltaErrors     = FALSE,
                          spDist          = 1,
                          drOnset         = 0,
                          drDist          = 0,
                          drShape         = 3,
                          drLim           = c(0.1, 0.7),
                          rtMax           = 5000,
                          costFunction    = "RMSE",
                          subjects        = c(),
                          printInputArgs  = TRUE,
                          printResults    = FALSE,
                          optimControl    = list(),
                          numCores        = 2,
                          setSeed         = TRUE,
                          seedValue       = 1234) {

  if (inherits(resOb, "dmcob")) {
    resOb <- list(resOb)
  }

  if (length(subjects) == 0) {
    subjects <- unique(resOb[[1]]$summarySubject$Subject) # fit all individual subjects in data
  }

  dmcfit <- vector("list", max(subjects))
  for (subject in subjects) {

    resObSubject <- list()
    for (i in 1:length(resOb)) {
      if (is.null(resOb[[i]]$data)) {
        resObSubject[[i]] <- list(
          deltaAgg = resOb[[i]]$deltaSubject[resOb[[i]]$deltaSubject$Subject == subject, ],
          cafAgg = resOb[[i]]$cafSubject[resOb[[i]]$cafSubject$Subject == subject, ]
        )
      } else {
        resObSubject[[i]] <- list(
          data = resOb[[i]]$data[resOb[[i]]$data$Subject == subject, ],
          deltaAgg = resOb[[i]]$deltaSubject[resOb[[i]]$deltaSubject$Subject == subject, ],
          cafAgg = resOb[[i]]$cafSubject[resOb[[i]]$cafSubject$Subject == subject, ]
        )
      }
    }

    dmcfit[[subject]] <- dmcFit(resObSubject,
                                nTrl            = nTrl,
                                startVals       = startVals,
                                minVals         = minVals,
                                maxVals         = maxVals,
                                fixedFit        = fixedFit,
                                freeCombined    = freeCombined,
                                fitInitialGrid  = fitInitialGrid,
                                fitInitialGridN = fitInitialGridN, # reduce if grid search 3/4+ parameters
                                fixedGrid       = fixedGrid, # only fit tau
                                nCAF            = nCAF,
                                nDelta          = nDelta,
                                pDelta          = pDelta,
                                tDelta          = tDelta,
                                deltaErrors     = deltaErrors,
                                costFunction    = costFunction,
                                spDist          = spDist,
                                drOnset         = drOnset,
                                drDist          = drDist,
                                drShape         = drShape,
                                drLim           = drLim,
                                rtMax           = rtMax,
                                printInputArgs  = printInputArgs,
                                printResults    = printResults,
                                optimControl    = optimControl,
                                numCores        = numCores
    )
  }

  if (length(resOb) == 1) {
    class(dmcfit) <- "dmcfit_subject"
  } else {
    class(dmcfit) <- "dmcfits_subject"
  }

  return(dmcfit)
}


#' @title dmcFitSubjectDE
#'
#' @description Fit theoretical data generated from dmcSim to observed data by
#' minimizing the root-mean-square error (RMSE) between a weighted combination
#' of the CAF and CDF functions using the R-package DEoptim. Alternative cost functions
#' include squared percentage error ("SPE"), and g-squared statistic ("GS").
#'
#' @param resOb Observed data (see flankerData and simonTask for data format)
#' @param nTrl The number of trials to use within dmcSim.
#' @param minVals Minimum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., minVals = list(amp = 10, tau = 5, drc = 0.1,
#' bnds = 20, resMean = 200, resSD = 5, aaShape = 1, spShape = 2, spBias = -20, sigm = 1, bndsRate=0, bndsSaturation=0)).
#' @param maxVals Maximum values for the to-be estimated parameters. This is a list with values specified individually
#' for amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., maxVals = list(amp = 40, tau = 300, drc = 1.0,
#' bnds = 150, bndsRate=1, bndsSaturation=500, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 20, sigm = 10))
#' @param fixedFit Fix parameter to starting value. This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, sigm (e.g., fixedFit = list(amp = FALSE,  tau = FALSE, drc = FALSE,
#' bnds = FALSE, bndsRate=TRUE, bndsSaturation=TRUE, resMean = FALSE, resSD = FALSE, aaShape = FALSE, spShape = FALSE, spBias = TRUE, sigm = TRUE, bndsRate=TRUE, bndsSaturation=TRUE))
#' NB. Value if fixed at midpoint between minVals and maxVals.
#' @param freeCombined If fitting 2+ datasets at once, which parameters are allowed to vary between both
#' fits (default = all parameters fixed between the two fits e.g. parameter = FALSE).
#' This is a list with bool values specified individually for
#' amp, tau, drc, bnds, resMean, resSD, aaShape, spShape, spBias, sigm (e.g., freeCombined = list(amp = FALSE,
#' tau = FALSE, drc = FALSE, bnds = FALSE, bndsRate=FALSE, bndsSaturation=FALSE, resMean = FALSE, resSD = FALSE, aaShape = FALSE, spShape = FALSE, spBias = FALSE, sigm = FALSE))
#' @param nCAF The number of CAF bins.
#' @param nDelta The number of delta bins.
#' @param pDelta An alternative option to nDelta (tDelta = 1 only) by directly specifying required percentile values (vector of values 0-100)
#' @param tDelta The type of delta calculation (1=direct percentiles points, 2=percentile bounds (tile) averaging)
#' @param deltaErrors TRUE/FALSE Calculate delta bins for error trials
#' @param costFunction The cost function to minimise: root mean square error ("RMSE": default),
#' squared percentage error ("SPE"), or likelihood-ratio chi-square statistic ("GS")
#' @param rtMax The limit on simulated RT (decision + non-decisional components)
#' @param spDist The starting point distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param drOnset The starting point of controlled drift rate (i.e., "target" information) relative to automatic ("distractor" incormation) (> 0 ms)
#' @param drDist The drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape The drift rate (dr) shape parameter
#' @param drLim The drift rate (dr) range
#' @param subjects NULL (aggregated data across all subjects) or integer for subject number
#' @param deControl Additional control parameters passed to DEoptim (see DEoptim.control)
#' @param numCores Number of cores to use
#' @param setSeed TRUE/FALSE (default = TRUE) Lock the random seed for deterministic simulations.
#' @param seedValue (default = 1234) The seed value used if setSeed is TRUE.
#'
#' @return dmcFitSubjectDE returns a list of objects of class "dmcfit"
#'
#' @examples
#' \donttest{
#' # Code below can exceed CRAN check time limit, hence donttest
#' # Example 1: Flanker data from Ulrich et al. (2015)
#' fit <- dmcFitSubjectDE(flankerData, nTrl = 1000, subjects = c(1, 2), deControl = list(itermax=30))
#' plot(fit, flankerData, subject = 1)
#' plot(fit, flankerData, subject = 2)
#' summary(fit)
#'
#' }
#'
#' @export
dmcFitSubjectDE <- function(resOb,
                            nTrl         = 100000,
                            minVals      = list(),
                            maxVals      = list(),
                            fixedFit     = list(),
                            freeCombined = list(),
                            nCAF         = 5,
                            nDelta       = 19,
                            pDelta       = vector(),
                            tDelta       = 1,
                            deltaErrors  = FALSE,
                            costFunction = "RMSE",
                            spDist       = 1,
                            drOnset      = 0,
                            drDist       = 0,
                            drShape      = 3,
                            drLim        = c(0.1, 0.7),
                            rtMax        = 5000,
                            subjects     = c(),
                            deControl    = list(),
                            numCores     = 2) {

  if (inherits(resOb, "dmcob")) {
    resOb <- list(resOb)
  }

  if (length(subjects) == 0) {
    subjects <- unique(resOb[[1]]$summarySubject$Subject) # fit all individual subjects in data
  }

  dmcfit <- vector("list", max(subjects))
  for (subject in subjects) {

    resObSubject <- list()
    for (i in 1:length(resOb)) {
      if (is.null(resOb[[i]]$data)) {
        resObSubject[[i]] <- list(
          deltaAgg = resOb[[i]]$deltaSubject[resOb[[i]]$deltaSubject$Subject == subject, ],
          cafAgg = resOb[[i]]$cafSubject[resOb[[i]]$cafSubject$Subject == subject, ]
        )
      } else {
        resObSubject[[i]] <- list(
          data = resOb[[i]]$data[resOb[[i]]$data$Subject == subject, ],
          deltaAgg = resOb[[i]]$deltaSubject[resOb[[i]]$deltaSubject$Subject == subject, ],
          cafAgg = resOb[[i]]$cafSubject[resOb[[i]]$cafSubject$Subject == subject, ]
        )
      }
    }

    dmcfit[[subject]] <- dmcFitDE(resObSubject,
      nTrl         = nTrl,
      minVals      = minVals,
      maxVals      = maxVals,
      fixedFit     = fixedFit,
      freeCombined = freeCombined,
      nCAF         = nCAF,
      nDelta       = nDelta,
      pDelta       = pDelta,
      tDelta       = tDelta,
      deltaErrors  = deltaErrors,
      costFunction = costFunction,
      spDist       = spDist,
      drOnset      = drOnset,
      drDist       = drDist,
      drShape      = drShape,
      drLim        = drLim,
      rtMax        = rtMax,
      deControl    = deControl,
      numCores     = numCores
    )
  }

  if (length(resOb) == 1) {
    class(dmcfit) <- "dmcfit_subject"
  } else {
    class(dmcfit) <- "dmcfits_subject"
  }

  return(dmcfit)
}


#' @title mean.dmcfit
#'
#' @description Aggregate simulation results from dmcFitSubject/dmcFitSubjectDE.
#'
#' @param x Output from dmcFitSubject/dmcFitSubjectDE
#' @param ... pars
#'
#' @return mean.dmcfit return an object of class "dmcfit" with the following components:
#' \item{summary}{DataFrame within aggregated subject data (rtCor, sdRtCor, seRtCor, perErr, sdPerErr, sePerErr, rtErr, sdRtErr, seRtErr) for compatibility condition}
#' \item{delta}{DataFrame within aggregated subject distributional delta analysis data correct trials (Bin, meanComp, meanIncomp, meanBin, meanEffect, sdEffect, seEffect)}
#' \item{caf}{DataFrame within aggregated subject conditional accuracy function (CAF) data (Bin, accPerComp, accPerIncomp, meanEffect, sdEffect, seEffect)}
#' \item{par}{The fitted model parameters + final cost value of the fit}
#'
#' @examples
#' \donttest{
#' # Code below can exceed CRAN check time limit, hence donttest
#' # Example 1: Fit individual data then aggregate
#' fitSubjects <- dmcFitSubject(flankerData, nTrl = 1000, subjects = c(1, 2))
#' fitAgg <- mean(fitSubjects)
#' plot(fitAgg, flankerData)
#' }
#'
#' @export
mean.dmcfit_subject <- function(x, ...) {

  mergeLists <- function(lists, name) {
    return(lapply(c(name), function(x) do.call(rbind, lapply(lists, `[[`, x)))[[1]])
  }

  # mean fit of summary, delta, caf, and parameters
  meanfit <- list()

  meanfit$summary <- mergeLists(x, "summary") %>%
    dplyr::group_by(Comp) %>%
    dplyr::summarise_all(mean)

  meanfit$delta <- mergeLists(x, "delta") %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarize_all(mean)

  meanfit$caf <- mergeLists(x, "caf") %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarize_all(mean)

  meanfit$par <- as.data.frame(mergeLists(x, "par")) %>%
    dplyr::summarize_all(~ mean(unlist(.x)))

  meanfit$sim <- dmcSim(amp = meanfit$par$amp, tau = meanfit$par$tau, drc = meanfit$par$drc,
    bnds = meanfit$par$bnds, resMean = meanfit$par$resMean, resSD = meanfit$par$resSD,
    aaShape = meanfit$par$aaShape, spShape = meanfit$par$spShape,
    spBias = meanfit$par$spBias, sigm = meanfit$par$sigm,
    printInputArgs = FALSE, printResults = FALSE)$sim

  class(meanfit) <- c("dmcfit")

  return(meanfit)
}

#' @title mean.dmcfits
#'
#' @description Aggregate simulation results across multiple datasets (models)
#'
#' @param x Output from dmcFit (class dmcfits)
#' @param ... pars
#'
#' @return an object of class "dmcfit"
#'
#' @export
mean.dmcfits <- function(x, ...) {
  # The structure of dmcfits (list of dataset fits) is identical 
  # to dmcfit_subject (list of subject fits) for the purposes of aggregation.
  return(mean.dmcfit_subject(x, ...))
}

#' @title mean.dmcfits_subject
#'
#' @description Aggregate simulation results across datasets and subjects
#'
#' @param x Output from dmcFitSubject (class dmcfits_subject)
#' @param ... pars
#'
#' @return an object of class "dmcfit"
#'
#' @export
mean.dmcfits_subject <- function(x, ...) {
  
  # 1. Aggregate across models/datasets for each participant
  subject_means <- lapply(x, function(subj_fits) {
    # Handle NULL subjects (missing data/failed fits)
    if (is.null(subj_fits)) return(NULL)
    return(mean.dmcfits(subj_fits, ...))
  })
  
  # Strip out any NULL subjects before across-participant aggregation
  subject_means <- subject_means[!sapply(subject_means, is.null)]
  
  # 2. Aggregate across participants
  class(subject_means) <- "dmcfit_subject"
  return(mean.dmcfit_subject(subject_means, ...))
}

# function to minimise
minimizeCostValue <- function(x,
                              costFunction,
                              prms,
                              fixedFit,
                              freeCombined,
                              resOb,
                              nTrl,
                              nDelta,
                              pDelta,
                              tDelta,
                              deltaErrors,
                              nCAF,
                              spDist,
                              drOnset,
                              drDist,
                              drShape,
                              drLim,
                              rtMax,
                              minVals,
                              maxVals,
                              printInputArgs,
                              printResults,
                              setSeed = FALSE,
                              seedValue = 1) {

  nParameters <- sum(as.logical(fixedFit) == FALSE)
  for (i in 1:length(prms)) {
    prms[[i]][!as.logical(fixedFit)] <- x[c(1:nParameters)]
  }
  if (length(prms) >= 2) {
    nFreeCombined <- sum(freeCombined == TRUE)
    start <- 1
    for (i in 2:length(prms)) {
      prms[[i]][as.logical(freeCombined)] <- x[-c(1:nParameters)][start:start+nFreeCombined-1]
      start <- start + nFreeCombined
    }
  }

  # implement bounds penalty
  penalty <- 0
  for (i in 1:length(prms)) {
    prms_vals <- unlist(prms[[i]])
    min_vals <- unlist(minVals)
    max_vals <- unlist(maxVals)
    
    out_of_bounds <- pmax(0, min_vals - prms_vals) + pmax(0, prms_vals - max_vals)
    if (sum(out_of_bounds) > 0) {
      penalty <- penalty + 1e6 + sum(out_of_bounds) * 1e6
    }
    
    prms[[i]] <- as.list(pmax(prms_vals, min_vals))
    prms[[i]] <- as.list(pmin(unlist(prms[[i]]), max_vals))
  }
  
  if (penalty > 0) {
    return(penalty)
  }

  resTh <- list()
  cost <- c()
  for (i in 1:length(resOb)) {
    cat(paste0("Data set ", i, ":"))
    resTh[[i]] <- dmcSim(
      amp = prms[[i]]$amp, tau = prms[[i]]$tau, drc = prms[[i]]$drc, bnds = prms[[i]]$bnds,
      resMean = prms[[i]]$resMean, resSD = prms[[i]]$resSD, aaShape = prms[[i]]$aaShape,
      spDist = spDist, spShape = prms[[i]]$spShape, spBias = prms[[i]]$spBias, sigm = prms[[i]]$sigm, spLim = c(-prms[[i]]$bnds, prms[[i]]$bnds),
      bndsRate = prms[[i]]$bndsRate,  bndsSaturation = prms[[i]]$bndsSaturation,
      drOnset = drOnset, drDist = drDist, drShape = drShape, drLim = drLim,
      rtMax = rtMax, nTrl = nTrl, nDelta = nDelta, pDelta = pDelta, tDelta = tDelta, deltaErrors = deltaErrors, nCAF = nCAF,
      printInputArgs = printInputArgs, printResults = printResults,
      setSeed = setSeed, seedValue = seedValue
    )
    cost[i] <- costFunction(resTh[[i]], resOb[[i]])
    cat(" | cost:", formatC(cost[i], 3, format = "f"), "\n")
  }

  return(sum(cost))

}

#' @title calculateCostValueRMSE
#'
#' @description Calculate cost value (fit) using root-mean-square error (RMSE) from a combination of RT and error rate.
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
#' cost <- calculateCostValueRMSE(resTh, resOb)
#'
#' # Example 2:
#' resTh <- dmcSim()
#' resOb <- dmcSim(tau = 150)
#' cost <- calculateCostValueRMSE(resTh, resOb)
#' @export
calculateCostValueRMSE <- function(resTh, resOb) {
  return(calculateCostValueRMSE_cpp(resTh, resOb))
}

#' @title calculateCostValueSPE
#'
#' @description Calculate cost value (fit) using squared percentage errror (SPE) from combination of RT and error rate.
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
#' cost <- calculateCostValueSPE(resTh, resOb)
#'
#' # Example 2:
#' resTh <- dmcSim()
#' resOb <- dmcSim(tau = 150)
#' cost <- calculateCostValueSPE(resTh, resOb)
#' @export
calculateCostValueSPE <- function(resTh, resOb) {
  return(calculateCostValueSPE_cpp(resTh, resOb))
}


#' @title calculateCostValueCS
#'
#' @description Calculate cost value (fit) using chi-square (CS) from correct and incorrect RT data.
#'
#' @param resTh list containing simulation $sim values (output from dmcSim) for rts_comp, rts_incomp,
#' errs_comp, errs_incomp
#' @param resOb list containing raw observed data (see dmcObservedData with keepRaw = TRUE)
#'
#' @return cost value (CS)
#'
#' @examples
#' # Example 1:
#' resTh <- dmcSim()
#' resOb <- flankerData
#' resOb <- calculateBinProbabilities(resOb)
#' cost  <- calculateCostValueCS(resTh, resOb)
#' @export
calculateCostValueCS <- function(resTh, resOb) {
  return(calculateCostValueCS_cpp(resTh, resOb))
}



#' @title calculateCostValueGS
#'
#' @description Calculate cost value (fit) using likelihood-ratio chi-square statistic (GS) from correct and incorrect RT data.
#'
#' @param resTh list containing simulation $sim values (output from dmcSim) for rts_comp, rts_incomp,
#' errs_comp, errs_incomp
#' @param resOb list containing raw observed data (see dmcObservedData with keepRaw = TRUE)
#'
#' @return cost value (GS)
#'
#' @examples
#' # Example 1:
#' resTh <- dmcSim()
#' resOb <- flankerData
#' resOb <- calculateBinProbabilities(resOb)
#' cost  <- calculateCostValueGS(resTh, resOb)
#' @export
calculateCostValueGS <- function(resTh, resOb) {
  return(calculateCostValueGS_cpp(resTh, resOb))
}


costValueFunction <- function(costFunction) {
  if (is.character(costFunction)) {
    if (tolower(costFunction) == "rmse") {
      return(calculateCostValueRMSE)
    } else if (tolower(costFunction) == "spe") {
      return(calculateCostValueSPE)
    } else if (tolower(costFunction) == "gs") {
      return(calculateCostValueGS)
    } else if (tolower(costFunction) == "cs") {
      return(calculateCostValueCS)
    }
  } else if (is.function(costFunction)) {
    return(costFunction)
  }
  stop("costFunction must be one of 'rmse', 'spe', 'gs', 'cs', or custom function")
}

#' @title calculateBinProbabilities
#'
#' @description Calculate bin probabilities in observed data
#'
#' @param resOb Observed data (see dmcObservedData)
#' @param quantileType Argument (1-9) from R function quantile specifying the algorithm (?quantile)
#'
#' @return resOb Observed data with additional $probSubject/$prob table
#'
#' @examples
#' # Example 1:
#' resOb <- flankerData
#' resOb <- calculateBinProbabilities(resOb)
#' resOb$prob
#' @export
calculateBinProbabilities <- function(resOb, quantileType = 5) {

  # Some subjects are likely not to have 5+ error trials, esp. in compatible conditions!
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)

  # dplyr 1.1.0 reframe replaces functionality from summarize
  # https://www.tidyverse.org/blog/2023/02/dplyr-1-1-0-pick-reframe-arrange/
  if (packageVersion("dplyr") >= "1.1.0") {
    resOb$probSubject <- resOb$data %>%
      dplyr::filter(outlier == 0) %>%
      dplyr::group_by(Subject, Comp, Error) %>%
      dplyr::reframe(
        nTrials  = n(),
        prob     = probs,
        boundary = quantile(RT, probs, quantileType = quantileType),
      ) %>%
      dplyr::filter(nTrials >= 5)
  } else {
    resOb$probSubject <- resOb$data %>%
      dplyr::filter(outlier == 0) %>%
      dplyr::group_by(Subject, Comp, Error) %>%
      dplyr::summarize(
        nTrials  = n(),
        prob     = probs,
        boundary = quantile(RT, probs, quantileType = quantileType),
        .groups  = "drop"
      ) %>%
      dplyr::filter(nTrials >= 5)
  }

  nTotal <- length(unique(resOb$probSubject$Subject))

  resOb$prob <- resOb$probSubject %>%
    dplyr::group_by(Comp, Error, prob) %>%
    dplyr::summarize(
      nSubjectsTotal = nTotal,
      nSubjectsGroup = n(),
      # nTrials        = mean(nTrials),
      nTrials        = sum(nTrials)/nTotal,
      boundary       = mean(boundary),
      .groups        = "drop"
    )

  return(resOb)
}
