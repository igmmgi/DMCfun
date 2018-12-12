#' @title dmcSim
#'
#' @description DMC model simulation detailed in  Ulrich, R., Schroeter, H., Leuthold, H., & Birngruber, T. (2015).
#' Automatic and controlled stimulus processing in conflict tasks: Superimposed diffusion processes and delta functions.
#' Cognitive Psychology, 78, 148-174. This function is essentially a wrapper around the c++ function runDMC
#'
#' @param amp amplitude of automatic activation
#' @param tau time to peak automatic activation
#' @param mu drift rate of controlled processes
#' @param bnds +- response barrier
#' @param resMean mean of non-decisional component
#' @param resSD standard deviation of non-decisional component
#' @param sigma diffusion constant
#' @param aaShape shape parameter of automatic activation
#' @param nTrl number of trials
#' @param tmax number of time points per trial
#' @param varSP true/false variable starting point
#' @param spShape shape parameter of starting point
#' @param spLim limit range of distribution of starting point
#' @param varDR true/false variable drift rate NB. In DMC, drift rate across trials is always
#' constant.
#' @param drShape shape parameter of drift rate
#' @param drLim limit range of distribution of drift rate
#' @param fullData TRUE/FALSE
#' @param nTrlData Number of trials to plot
#' @param stepDelta Number of delta bins
#' @param stepCAF Number of CAF bins
#' @param printInputArgs TRUE/FALSE
#' @param printResults TRUE/FALSE
#' @param setSeed TRUE/FALSE
#'
#' @return dmcsim
#'
#' The function returns a list with the relevant results from the simulation. The list
#' is accessed with obj$name and so on with the the following:
#' \item{obj$summary}{summary}
#' \item{obj$delta}{delta}
#' \item{obj$caf}{caf}
#' \item{obj$sim}{sim}
#' \item{obj$trials}{trials}
#' \item{obj$prms}{prms}
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1
#' dmc <- dmcSim(fullData = TRUE)
#' plot(dmc)
#'
#' # Example 2
#' dmc <- dmcSim(fullData = TRUE, tau = 130)
#' plot(dmc)
#'
#' # Example 3
#' dmc <- dmcSim(fullData = TRUE, tau = 90)
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
#' dmc <- dmcSim(fullData = TRUE, stepDelta = 10, stepCAF = 25)
#' plot(dmc)}
#'
#' @export
dmcSim <- function(amp = 20, tau = 30, mu = 0.5, bnds = 75, resMean = 300, resSD = 30,
                   sigma = 4, aaShape = 2,
                   nTrl = 100000, tmax = 1000,
                   varSP = FALSE, spShape = 3, spLim = c(-75, 75),
                   varDR = FALSE, drShape = 3, drLim = c(0.1, 0.7),
                   fullData = FALSE, nTrlData = 5,
                   stepDelta = 5, stepCAF = 20,
                   printInputArgs = TRUE, printResults = TRUE,
                   setSeed = FALSE) {

  dmc = dmcCppR(r_in = list(amp = amp, tau = tau, mu = mu, bnds = bnds, resMean = resMean, resSD = resSD,
                            sigma = sigma, aaShape = aaShape,
                            nTrl = nTrl, tmax = tmax,
                            varSP = varSP, spShape = spShape, spLimLow = spLim[1], spLimHigh = spLim[2],
                            varDR = varDR, drShape = drShape, drLimLow = drLim[1], drLimHigh = drLim[2],
                            fullData = fullData, nTrlData = nTrlData,
                            stepDelta = stepDelta, stepCAF = stepCAF,
                            printInputArgs = printInputArgs, printResults = printResults,
                            setSeed = setSeed))

  # summary
  dmc$summary <- tibble::as_tibble(rbind(dmc$summary$resSum_comp, dmc$summary$resSum_incomp))
  colnames(dmc$summary) <- c("rtCor", "sdRtCor", "perErr", "rtErr", "sdRtErr")
  dmc$summary <- tibble::add_column(Comp = c("comp", "incomp"), dmc$summary, .before = TRUE)

  # caf
  nCAF    <- length(dmc$caf$caf_comp)
  dmc$caf <- tibble::tibble(accPer = c(dmc$caf$caf_comp, dmc$caf$caf_incomp))
  dmc$caf <- tibble::add_column(bin = rep(1:nCAF, each = 1, times = 2), dmc$caf, .before = TRUE)
  dmc$caf <- tibble::add_column(Comp = rep(c("comp", "incomp"), each = nCAF), dmc$caf, .before = TRUE)

  # delta
  nDelta    <- length(dmc$delta$delta_pct_comp)
  dmc$delta <- tibble::tibble("meanComp" = dmc$delta$delta_pct_comp,
                              "meanIncomp" = dmc$delta$delta_pct_incomp,
                              "meanBin" = dmc$delta$delta_pct_mean,
                              "meanEffect" = dmc$delta$delta_pct_delta)
  dmc$delta <- tibble::add_column("Bin" = rep(1:nDelta, each = 1, times = 1), dmc$delta, .before = TRUE)

  # store parameters used to call function
  dmc$prms <- modifyList(as.list(formals(dmcSim)), as.list(sys.call()))

  class(dmc) <- "dmcsim"
  return(dmc)

}
