#' @title summary.dmcfit
#'
#' @description Summary of the simulation results from dmcFitAgg/dmcFitVPs.
#'
#' @param object Output from dmcFit
#' @param ... pars
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' # Example 1
#' fitAgg <- dmcFitAgg(flankerData, nTrl = 1000)
#' summary(fitAgg)
#' plot(fitAgg, flankerData)
#'
#' # Example 2
#' fitVPs <- dmcFitVPs(flankerData, nTrl = 1000, VP = c(1, 10))
#' summary(fitVPs)
#' fit <- mean(fitVPs)
#' plot(fit, flankerData)
#' }
#'
#' @export
summary.dmcfit <- function(object, ...) {

  # if ("par" %in% names(object[[2]])) {
  if ("par" %in% names(object)) {
    
    # out <- tibble::as_tibble(object[[2]]$par)
    out <- tibble::as_tibble(object$par)

  } else {
  
    VPs <- which(!unlist(lapply(object, is.null)))
    outVP <- NULL
    for (VP in VPs) {
      # outVP <- rbind(outVP, cbind(VP, tibble::as_tibble(object[[VP]][[2]]$par)))
      outVP <- rbind(outVP, cbind(VP, tibble::as_tibble(object[[VP]]$par)))
    }
    outAvg <- tibble::as_tibble(t(c(colMeans(outVP[, c(2:11)]))))
    
    out    <- list(outVP, outAvg)

  }

  return(out)

}
