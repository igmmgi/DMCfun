#' @title plot.dmclist
#'
#' @description Plot the simulation results from the output of dmcSim. The plot
#' can be an overall summary, or individual plots (activation, trials, pdf, cdf,
#' caf, delta, all). Plot type summary1 contains an activation plot, example
#' individual trials, the probaility distribution function (PDF), the cumulative
#' distribution function (CDF), the conditional accuracy functin (CAF) and
#' delta plot. This requires that dmcSim is run with fullData = TRUE. Plot type
#' summary2 contains only the PDF, CDF, CAF and delta plots and does not require
#' that dmcSim is run with fullData = TRUE.
#'
#' @param x Output from dmcSims
#' @param ylimit ylimit for delta plot
#' @param xlimit xlimit for delta plot
#' @param col # color range start/end color
#' @param lineType line type ("l", "b", "o") for delta plot
#' @param legendPos legend position
#' @param ... pars for legend
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1
#' params <- list(amp = seq(20, 30, 2), tau = c(20, 250))
#' dmc = dmcSims(params)
#' plot(dmc)
#'
#' # Example 2
#' params <- list(amp=c(10, 20), tau = seq(20, 100, 20), mu = seq(0.2, 0.5, 0.1))
#' dmc = dmcSims(params)
#' plot(dmc, ncol = 3, col=c("green", "blue"), lineType = "l")
#'
#' }
#'
#' @export
plot.dmclist <- function(x,
                         ylimit = c(-50, 150),
                         xlimit = NULL,
                         col=c("black", "lightgrey"),
                         lineType = "l",
                         legendPos = "topleft",
                         ...) {

  # default xlimit
  if (is.null(xlimit)) {
    xlimit <- c(0, x[[1]]$prms$tmax)
  }

  # color range
  cols <- colorRampPalette(col)(length(x))

  # plot
  plot(x[[1]]$delta$meanBin, x[[1]]$delta$meanEffect,
       xlim = xlimit, xlab = "Time [ms]",
       ylab = expression(Delta), ylim = ylimit, type = lineType, col = cols[1])
  legendText <- paste0(paste0(names(x[[1]]$params[[1]]), "=", x[[1]]$params[[1]], collapse = ", "))
  for (i in 2:length(x)) {
    lines(x[[i]]$delta$meanBin, x[[i]]$delta$meanEffect, col = cols[i], type = lineType)
    legendText <- c(legendText, paste0(names(x[[i]]$params[[i]]), "=", x[[i]]$params[[i]], collapse = ", "))
  }
  legend(legendPos, legend = legendText, col=cols, lty=1, ...)
}
