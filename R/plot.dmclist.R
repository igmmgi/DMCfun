#' @title plot.dmclist
#'
#' @description Plot the simulation results from the output of dmcSim. The plot
#' can be an overall summary, or individual plots (activation, trials, pdf, cdf,
#' caf, delta, all). Plot type summary1 contains an activation plot, example
#' individual trials, the probability distribution function (PDF), the cumulative
#' distribution function (CDF), the conditional accuracy function (CAF) and
#' delta plot. This requires that dmcSim is run with fullData = TRUE. Plot type
#' summary2 contains only the PDF, CDF, CAF and delta plots and does not require
#' that dmcSim is run with fullData = TRUE.
#'
#' @param x Output from dmcSims
#' @param ylim ylimit for delta plot
#' @param xlim xlimit for delta plot
#' @param col # color range start/end color
#' @param lineType line type ("l", "b", "o") for delta plot
#' @param legendPos legend position
#' @param ... pars for legend
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' # Example 1
#' params <- list(amp = seq(20, 30, 2))
#' dmc <- dmcSims(params)
#' plot(dmc, ncol = 2, xlim = c(0, 1300), ylim = c(-100, 200))
#'
#' # Example 2
#' params <- list(amp=c(10, 20), tau = seq(20, 80, 40), mu = seq(0.2, 0.6, 0.2), nTrl = 50000)
#' dmc <- dmcSims(params)
#' plot(dmc, ncol = 2, col=c("green", "blue"), lineType = "l")
#'
#' }
#'
#' @export
plot.dmclist <- function(x,
                         ylim = c(-50, 150),
                         xlim = NULL,
                         col=c("black", "lightgrey"),
                         lineType = "l",
                         legendPos = "topleft",
                         ...) {

  # default xlimit
  if (is.null(xlim)) {
    xlim <- c(0, x[[1]]$prms$tmax)
  }

  # colour range
  cols <- colorRampPalette(col)(length(x))

  # plot
  plot(x[[1]]$delta$meanBin, x[[1]]$delta$meanEffect, type = lineType,
       ylim = ylim, xlim = xlim,
       ylab = expression(Delta), xlab = "Time [ms]", col = cols[1])

  legendText <- paste0(names(x[[1]]$params), "=", x[[1]]$params[1, ], collapse = ", ")
  for (i in 2:length(x)) {
    lines(x[[i]]$delta$meanBin, x[[i]]$delta$meanEffect, col = cols[i], type = lineType)
    legendText <- c(NULL, legendText, paste0(names(x[[i]]$params), "=", x[[1]]$params[i, ], collapse = ", "))
  }
  legend(legendPos, legend = legendText, col = as.vector(cols), lty = 1, ...)
}
