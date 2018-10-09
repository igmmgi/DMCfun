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
#' library(DMCfun)
#'
#' # Example 1
#' fitAgg <- dmcFitAgg(flankerData1, nTrl = 1000)
#' summary(fitAgg)
#' plot(fitAgg, flankerData1)
#'
#' # Example 2
#' fitVPs <- dmcFitVPs(flankerData1, nTrl = 1000)
#' summary(fitVPs)
#' fit <- mean(fitVPs)
#' plot(fit, flankerData1)
#' }
#'
#' @export
summary.dmcfit <- function(object, ...) {

  if ("par" %in% names(object[[2]])) {

    out           <- tibble::as_tibble(cbind(t(object[[2]][[1]]), object[[2]][[2]]))
    colnames(out) <- c("amp", "tau", "mu", "bnds", "resMean", "resSD", "spShape", "rmse")

  } else {

    outVP <- tibble::as_tibble(cbind(VP = which(!unlist(lapply(object, is.null))),
                                     do.call(rbind, lapply(lapply(object, `[[`, 2), `[[`, 1)),
                                     do.call(rbind, lapply(lapply(object, `[[`, 2), `[[`, 2))))
    outAvg <- tibble::as_tibble(t(c(VP = NA,  colMeans(outVP[,c(2:9)]))))

    out <- rbind(outVP, outAvg)
    colnames(out) <- c("VP", "amp", "tau", "mu", "bnds", "resMean", "resSD", "spShape", "rmse")

  }

  return(out)

}
