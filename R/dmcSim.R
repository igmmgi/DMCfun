#' @title dmcSim: Run dmc simulation 
#'
#' @description DMC model simulation detailed in  Ulrich, R., Schroeter, H., Leuthold, H., & Birngruber, T. (2015).
#' Automatic and controlled stimulus processing in conflict tasks: Superimposed diffusion processes and delta functions.
#' Cognitive Psychology, 78, 148-174. This function is essentially a wrapper around the c++ function runDMC
#'
#' @param amp amplitude of automatic activation
#' @param tau time to peak automatic activation
#' @param drc drift rate of controlled processes
#' @param bnds +- response criterion
#' @param resMean mean of non-decisional component
#' @param resSD standard deviation of non-decisional component
#' @param rtMax limit on simulated RT (decision + non-decisional component)
#' @param sigm diffusion constant
#' @param aaShape shape parameter of automatic activation
#' @param nTrl number of trials
#' @param tmax number of time points per trial
#' @param varSP true/false variable starting point
#' @param spShape shape parameter of starting point
#' @param spLim limit range of distribution of starting point
#' @param varDR true/false variable drift rate NB. In DMC, drift rate across trials is always constant.
#' @param drShape shape parameter of drift rate
#' @param drLim limit range of distribution of drift rate
#' @param fullData TRUE/FALSE (Default: FALSE)
#' @param nTrlData Number of trials to plot
#' @param nDelta Number of delta bins
#' @param pDelta Alternative to nDelta by directly specifying required percentile values   
#' @param nCAF Number of CAF bins
#' @param printInputArgs TRUE/FALSE
#' @param printResults TRUE/FALSE
#' @param setSeed TRUE/FALSE
#'
#' @return dmcsim
#'
#' The function returns a list with the relevant results from the simulation. The list
#' is accessed with obj$name with the the following:
#' \item{obj$means}{Condition means for reaction time and error rate}
#' \item{obj$caf}{Accuracy per bin for compatible and incompatible trials}
#' \item{obj$delta}{Mean RT and compatibility effect per bin}
#' \item{obj$sim}{Individual trial data points (reaction times/error) and activation vectors from simulation}
#' \item{obj$trials}{Example individual trial timecourse for n compatible and incompatible trials}
#' \item{obj$prms}{The input parameters used in the simulation}
#'
#' @examples
#' \donttest{
#' # Example 1
#' dmc <- dmcSim(fullData = TRUE)  # full data only required for activation plot (top left)
#' plot(dmc)
#' dmc <- dmcSim() # faster
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
#' dmc <- dmcSim(varSP = TRUE)
#' plot(dmc, "delta")
#'
#' # Example 5
#' dmc <- dmcSim(tau = 130, varDR = TRUE)
#' plot(dmc, "caf")
#'
#' # Example 6
#' dmc <- dmcSim(nDelta = 10, nCAF = 10)
#' plot(dmc)
#' }
#'
#' @export
dmcSim <- function(amp = 20, tau = 30, drc = 0.5, bnds = 75, resMean = 300, resSD = 30, aaShape = 2, spShape = 3,
                   sigm = 4,  nTrl = 100000, tmax = 1000,
                   varSP = FALSE, spLim = c(-75, 75),
                   varDR = FALSE, drShape = 3, drLim = c(0.1, 0.7), 
                   rtMax = 5000, 
                   fullData = FALSE, nTrlData = 5,
                   nDelta = 9, pDelta = vector(), nCAF = 5,
                   printInputArgs = TRUE, printResults = TRUE,
                   setSeed = FALSE) {

  # change nDelta to length of pDelta if pDelta not empty
  if (length(pDelta) != 0) {
    nDelta = length(pDelta)
  }
  
  dmc <- dmcCppR(r_in = list(amp = amp, tau = tau, drc = drc, bnds = bnds, resMean = resMean, resSD = resSD, aaShape = aaShape, spShape = spShape,
                             sigm = sigm,  nTrl = nTrl, tmax = tmax,
                             varSP = varSP, spLimLow = spLim[1], spLimHigh = spLim[2],
                             varDR = varDR, drShape = 3, drLimLow = drLim[1], drLimHigh = drLim[2], 
                             rtMax = rtMax, 
                             fullData = fullData, nTrlData = nTrlData,
                             nDelta = nDelta, pDelta = pDelta, nCAF = nCAF,
                             printInputArgs = printInputArgs, printResults = printResults,
                             setSeed = setSeed))
  
  summary     <- dmc$summary 
  dmc$summary <- NULL
  
  # means
  dmc$summary        <- as.data.frame(rbind(summary$resSum_comp, summary$resSum_incomp)) 
  names(dmc$summary) <- c("rtCor", "sdRtCor", "perErr", "rtErr", "sdRtErr", "perSlow")
  dmc$summary        <- cbind(Comp = c("comp", "incomp"), dmc$summary)

  # caf
  dmc$caf <- cbind(Comp = rep(c("comp", "incomp"), each = nCAF), 
                   as.data.frame(cbind(bin    = as.numeric(rep(1:nCAF, each = 1, times = 2)), 
                                       accPer =  as.numeric(c(summary$caf_comp, summary$caf_incomp)))))
  
  # delta
  dmc$delta <- as.data.frame(cbind(Bin        = rep(1:nDelta, each = 1, times = 1),
                                   meanComp   = summary$delta_pct_comp, 
                                   meanIncomp = summary$delta_pct_incomp, 
                                   meanBin    = summary$delta_pct_mean,
                                   meanEffect = summary$delta_pct_delta))
  
  # store parameters used to call function
  dmc$prms <- as.list(environment())
  
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
#' @return list of dmcsim
#'
#' @examples
#' \donttest{
#' # Example 1
#' params <- list(amp = seq(10, 20, 5), tau = c(50, 100, 150), nTrl = 50000)
#' dmc <- dmcSims(params)
#' plot(dmc[[1]])    # full combination 1
#' plot(dmc)         # delta plots for all combinations
#' plot(dmc[c(1:3)]) # delta plots for specific combinations
#'
#' # Example 2
#' params <- list(amp = seq(10, 20, 5), tau = seq(20, 40, 20), bnds = seq(50, 100, 25))
#' dmc <- dmcSims(params)
#' plot(dmc[[1]])  # combination 1
#' plot(dmc, ncol = 2)       # delta plots for all combinations
#' plot(dmc[c(1:3)]) # delta plots for specific combinations
#' }
#'
#' @export
dmcSims <- function(params,
                    printInputArgs = FALSE,
                    printResults = FALSE) {

  params  <- expand.grid(params)
  if (ncol(params) > 1) {
    uparams <- params[, lengths(lapply(params, unique)) != 1]
  } else {
    uparams <- params
  }
  params <- setNames(split(params, seq(nrow(params))), rownames(params))

  dmc <- vector("list", length(params))
  for (i in 1:length(params)) {

    # inputs for each dmcSim call taken from params + add default of not printing individual results
    dmcInputs <- params[[i]]
    message("DMC ", i, " of ", length(params), ": ", paste0(names(dmcInputs), "=", dmcInputs, sep="", collapse=", "))
    dmcInputs$printInputArgs <- printInputArgs
    dmcInputs$printResults   <- printResults

    dmc[[i]]        <- do.call(dmcSim, dmcInputs)
    dmc[[i]]$params <- uparams

  }

  class(dmc) <- "dmclist"
  return(dmc)

}

