#' @title dmcSim
#'
#' @description DMC model simulation detailed in  Ulrich, R., Schroeter, H., Leuthold, H., & Birngruber, T. (2015).
#' Automatic and controlled stimulus processing in conflict tasks: Superimposed diffusion processes and delta functions.
#' Cognitive Psychology, 78, 148-174. This function is essentially a wrapper around the c++ function runDMC
#'
#' @param amp amplitude of automatic activation
#' @param tau time to peak automatic activation
#' @param drc drift rate of controlled processes
#' @param bnds +- response criterion
#' @param resDist residual distribution type (1=normal, 2=uniform)
#' @param resMean residual distribution mean
#' @param resSD residual distribution standard deviation
#' @param rtMax limit on simulated RT (decision + non-decisional component)
#' @param sigm diffusion constant
#' @param aaShape shape parameter of automatic activation
#' @param nTrl number of trials
#' @param tmax number of time points per trial
#' @param spDist starting point (sp) distribution (0 = constant, 1 = beta, 2 = uniform)
#' @param spShape starting point (sp) shape parameter
#' @param spLim starting point (sp) range
#' @param spBias starting point (sp) bias
#' @param drOnset drift rate (dr) onset (default=0; must be >= 0)
#' @param drDist drift rate (dr) distribution type (0 = constant, 1 = beta, 2 = uniform)
#' @param drShape drift rate (dr) shape parameter
#' @param drLim drift rate (dr) range
#' @param fullData TRUE/FALSE (Default: FALSE) NB. only required when plotting activation
#' function and/or individual trials
#' @param nTrlData Number of trials to plot
#' @param nDelta number of delta bins
#' @param pDelta alternative to nDelta (tDelta = 1 only) by directly specifying required percentile values (0-100)
#' @param tDelta type of delta calculation (1=direct percentiles points, 2=percentile bounds (tile) averaging)
#' @param deltaErrors TRUE/FALSE Calculate delta bins for error trials
#' @param nCAF Number of CAF bins
#' @param bndsRate 0 (default) = fixed bnds
#' @param bndsSaturation bndsSaturatoin
#' @param printInputArgs TRUE/FALSE
#' @param printResults TRUE/FALSE
#' @param setSeed TRUE/FALSE If true, set seed to seed value
#' @param seedValue 1
#'
#' @return dmcSim returns an object of class "dmcsim" with the following components:
#' \item{sim}{Individual trial data points (reaction times/error) and activation vectors from simulation}
#' \item{summary}{Condition means for reaction time and error rate}
#' \item{caf}{Accuracy per bin for compatible and incompatible trials}
#' \item{delta}{Mean RT and compatibility effect per bin}
#' \item{deltaErrors}{Optional output: Mean RT and compatibility effect per bin for error trials}
#' \item{prms}{The input parameters used in the simulation}
#'
#' @examples
#' \donttest{
#' # Example 1
#' dmc <- dmcSim(fullData = TRUE) # fullData only needed for activation/trials (left column plot)
#' plot(dmc)
#' dmc <- dmcSim() # faster!
#' plot(dmc)
#'
#' # Example 2
#' dmc <- dmcSim(tau = 130)
#' plot(dmc)
#'
#' # Example 3
#' dmc <- dmcSim(tau = 90)
#' plot(dmc)
#'
#' # Example 4
#' dmc <- dmcSim(spDist = 1)
#' plot(dmc, "delta")
#'
#' # Example 5
#' dmc <- dmcSim(tau = 130, drDist = 1)
#' plot(dmc, "caf")
#'
#' # Example 6
#' dmc <- dmcSim(nDelta = 10, nCAF = 10)
#' plot(dmc)
#' }
#'
#' @export
dmcSim <- function(amp = 20,
                   tau = 30,
                   drc = 0.5,
                   bnds = 75,
                   resDist = 1,
                   resMean = 300,
                   resSD = 30,
                   aaShape = 2,
                   spShape = 3,
                   sigm = 4,
                   nTrl = 100000,
                   tmax = 1000,
                   spDist = 0,
                   spLim = c(-75, 75),
                   spBias = 0,
                   drOnset = 0,
                   drDist = 0,
                   drShape = 3,
                   drLim = c(0.1, 0.7),
                   rtMax = 5000,
                   fullData = FALSE,
                   nTrlData = 5,
                   nDelta = 9,
                   pDelta = vector(),
                   tDelta = 1,
                   deltaErrors = FALSE,
                   nCAF = 5,
                   bndsRate=0,
                   bndsSaturation=0,
                   printInputArgs = TRUE,
                   printResults = TRUE,
                   setSeed = FALSE,
                   seedValue = 1
                   ) {


  # default parameter space
  defaultStartVals    <- list(amp = 20, tau = 200, drc = 0.5, bnds = 75,  bndsRate=0, bndsSaturation=0,   resMean = 300, resSD = 30,  aaShape = 2, spShape = 3, spBias = 0,   sigm = 4)
  defaultMinVals      <- list(amp = 0,  tau = 5,   drc = 0.1, bnds = 20,  bndsRate=0, bndsSaturation=0,   resMean = 200, resSD = 5,   aaShape = 1, spShape = 2, spBias = -20, sigm = 1)
  defaultMaxVals      <- list(amp = 40, tau = 300, drc = 1.0, bnds = 150, bndsRate=1, bndsSaturation=500, resMean = 800, resSD = 100, aaShape = 3, spShape = 4, spBias = 20,  sigm = 10)
  defaultFixedFit     <- list(amp = F,  tau = F,   drc = F,   bnds = F,   bndsRate=T, bndsSaturation=T,   resMean = F,   resSD = F,   aaShape = F, spShape = F, spBias = T,   sigm = T)
  defaultFixedGrid    <- list(amp = T,  tau = F,   drc = T,   bnds = T,   bndsRate=T, bndsSaturation=T,   resMean = T,   resSD = T,   aaShape = T, spShape = T, spBias = T,   sigm = T)
  defaultFreeCombined <- list(amp = F,  tau = F,   drc = F,   bnds = F,   bndsRate=F, bndsSaturation=F,   resMean = F,   resSD = F,   aaShape = F, spShape = F, spBias = F,   sigm = F)

  # change nDelta to length of pDelta if pDelta not empty
  if (length(pDelta) != 0) {
    nDelta <- length(pDelta)
    tDelta <- 1
  }

  # call to cpp function for the simulation
  dmc <- dmcCppR(
    r_in = list(
      amp = amp, tau = tau, drc = drc, bnds = bnds, resDist = resDist,
      resMean = resMean, resSD = resSD, aaShape = aaShape, spShape = spShape,
      spBias = spBias, sigm = sigm, nTrl = nTrl, tmax = tmax, rtMax = rtMax,
      fullData = fullData, nTrlData = nTrlData, nDelta = nDelta,
      pDelta = pDelta, tDelta = tDelta, deltaErrors = deltaErrors,
      nCAF = nCAF, spDist = spDist, spLimLow = spLim[1], spLimHigh = spLim[2],
      drOnset = drOnset, drDist = drDist, drShape = drShape,
      drLimLow = drLim[1], drLimHigh = drLim[2], bndsRate=bndsRate, bndsSaturation=bndsSaturation,
      printInputArgs = printInputArgs, printResults = printResults,
      setSeed = setSeed, seedValue = seedValue
    )
  )

  summary <- dmc$summary
  dmc$summary <- NULL

  # means
  dmc$summary        <- as.data.frame(rbind(summary$comp, summary$incomp))
  names(dmc$summary) <- c("rtCor", "sdRtCor", "perErr", "rtErr", "sdRtErr", "perSlow")
  dmc$summary        <- cbind(Comp = c("comp", "incomp"), dmc$summary)

  # caf
  dmc$caf <- as.data.frame(cbind(
    Bin          = 1:nCAF,
    accPerComp   = summary$caf_comp,
    accPerIncomp = summary$caf_incomp,
    meanBin      = (summary$caf_rt_comp + summary$caf_rt_incomp) / 2,
    meanEffect   = ((100 - summary$caf_incomp) - (100 - summary$caf_comp)) * 100
  ))

  # delta
  dmc$delta <- as.data.frame(cbind(
    Bin        = 1:nDelta,
    meanComp   = summary$delta_correct_comp,
    meanIncomp = summary$delta_correct_incomp,
    meanBin    = summary$delta_correct_mean,
    meanEffect = summary$delta_correct_delta
  ))

  if (deltaErrors) {
    dmc$deltaErrors <- as.data.frame(cbind(
      Bin        = 1:nDelta,
      meanComp   = summary$delta_errors_comp,
      meanIncomp = summary$delta_errors_incomp,
      meanBin    = summary$delta_errors_mean,
      meanEffect = summary$delta_errors_delta
    ))
  }

  # store parameters used to call function
  dmc$prms <- data.frame(
    amp = amp, tau = tau, drc = drc, bnds = bnds, resMean = resMean,
    resSD = resSD, aaShape = aaShape, spShape = spShape, spBias = spBias,
    sigm = sigm, bndsRate = bndsRate, bndsSaturation = bndsSaturation,
    nTrl = nTrl, nTrlData = nTrlData, tmax = tmax,
    resDist = resDist, spDist = spDist, spLim1 = spLim[1], spLim2 = spLim[2],
    drOnset = drOnset, drDist = drDist, drShape = drShape,
    drLim1 = drLim[1], drLim2 = drLim[2]
  )

  class(dmc) <- "dmcsim"

  return(dmc)
}


#' @title dmcSims: Run multiple dmc simulations
#'
#' @description Run dmcSim with range of input parameters.
#'
#' @param params (list of parameters to dmcSim)
#' @param printInputArgs Print DMC input arguments to console
#' @param printResults Print DMC output to console
#'
#' @return dmcSims returns a list of objects of class "dmcsim"
#'
#' @examples
#' \donttest{
#' # Example 1
#' params <- list(amp = seq(10, 20, 5), tau = c(50, 100, 150), nTrl = 50000)
#' dmc <- dmcSims(params)
#' plot(dmc[[1]]) # full combination 1
#' plot(dmc) # delta plots for all combinations
#' plot(dmc[c(1:3)]) # delta plots for specific combinations
#' plot(dmc[c(1, 3)]) # delta plots for specific combinations
#'
#' # Example 2
#' params <- list(amp = seq(10, 20, 5), tau = seq(20, 40, 20), bnds = seq(50, 100, 25))
#' dmc <- dmcSims(params)
#' plot(dmc[[1]]) # combination 1
#' plot(dmc, ncol = 2) # delta plots for all combinations
#' plot(dmc[c(1:3)]) # delta plots for specific combinations
#' }
#'
#' @export
dmcSims <- function(params, printInputArgs = FALSE, printResults = FALSE) {
  params  <- expand.grid(params)
  uparams <- params
  params  <- setNames(split(params, seq(nrow(params))), rownames(params))

  dmc <- vector("list", length(params))
  for (i in seq_along(params)) {
    # inputs for each dmcSim call taken from params + add default of not printing individual results
    dmcInputs <- params[[i]]
    message("DMC ", i, " of ", length(params), ": ", paste0(names(dmcInputs), "=", dmcInputs, collapse = ", "))
    dmcInputs$printInputArgs <- printInputArgs
    dmcInputs$printResults <- printResults

    dmc[[i]]        <- do.call(dmcSim, dmcInputs)
    dmc[[i]]$params <- params[[i]][lengths(lapply(uparams, unique)) != 1]
  }

  class(dmc) <- "dmclist"
  return(dmc)
}


#' @title dmcSimApp
#' @description A shiny app allowing interactive exploration of DMC parameters
#' @return Shiny App
#'
#' @export
dmcSimApp <- function() {
  if (!requireNamespace("shiny")) {
    stop("This addin requires the 'shiny' package.")
  }
  shiny::runApp(system.file("dmcSimApp", package = "DMCfun"))
}
