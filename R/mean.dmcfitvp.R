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
#' \dontrun{
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
