#' @title mean.dmcfit
#'
#' @description Mean of the simulation results from dmcFitAgg/dmcFitVPs.
#'
#' @param x Output from dmcFit
#' @param ... pars
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1: Fit individual data then aggregate
#' fitVPs <- dmcFitVPs(flankerData, nTrl = 1000)
#' summary(fitVPs)
#' plot(fitVPs, flankerData, VP = 1)
#' fitAgg <- mean(fitVPs)
#' summary(fitAgg)
#' plot(fitAgg, flankerData)
#' }
#'
#' @export
mean.dmcfit <- function(x, ...) {

  resTh <- list()
  fit <- list()
  if (length(x) > 1) {

    # summary
    resTh$summary <- do.call(rbind, lapply(lapply(x, `[[`, 1), `[[`, 1)) %>%
      dplyr::group_by(Comp) %>%
      dplyr::summarise(rtCor   = mean(rtCor),
                       sdCor   = mean(sdRtCor),
                       perErr  = mean(perErr),
                       rtErr   = mean(rtErr),
                       sdRtErr = mean(sdRtErr))

    # delta
    resTh$delta <- do.call(rbind, lapply(lapply(x, `[[`, 1), `[[`, 2)) %>%
      dplyr::group_by(Bin) %>%
      dplyr::summarise(meanComp   = mean(meanComp),
                       meanIncomp = mean(meanIncomp),
                       meanBin    = mean(meanBin),
                       meanEffect = mean(meanEffect))

    # caf
    resTh$caf <- do.call(rbind, lapply(lapply(x, `[[`, 1), `[[`, 3)) %>%
      dplyr::group_by(Comp, bin) %>%
      dplyr::summarise(accPer = mean(accPer))

    # par
    fit$par <- colMeans(do.call(rbind, lapply(lapply(x, `[[`, 2), `[[`, 1)))

    # rmse
    fit$value <- colMeans(do.call(rbind, lapply(lapply(x, `[[`, 2), `[[`, 2)))

    out <- list(resTh, fit)

  }

  class(out) <- c("dmcfit")
  return(out)

}
