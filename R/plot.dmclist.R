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
#' @param ylimitDelta
#' @param errorBars TRUE/FALSE
#' @param ... pars
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1
#' dmc = dmcSims()
#' plot(dmc)
#'
#' }
#'
#' @export
plot.dmclist <- function(x,
                         ylimitDelta = c(-50, 150),
                         col=c("black", "lightgrey"),
                         ppars = ...,
                         lpars = list()) {

    cols <- colorRampPalette(col)(length(x))

    # plot(x[[1]], figType = "delta", col = cols[1], type = "l", dots[names(dots) %in% pnames])
    do.call(plot, x[[1]], figType = "delta", col = cols[1], type = "l", ppars)
    #plot(x[[1]], figType = "delta", col = cols[1], type = "l")
    legendText <- paste0(paste0(names(x[[1]]$params[[1]]), "=", x[[1]]$params[[1]], collapse = ", "))
    for (i in 2:length(x)) {
      lines(x[[i]]$delta$meanBin, x[[i]]$delta$meanEffect, col = cols[i])
      legendText <- c(legendText, paste0(paste0(names(x[[i]]$params[[i]]), "=", x[[i]]$params[[i]], collapse = ", ")))
    }
    # legend(1, 150, legend = legendText, col=cols, lty=1, ...)
}

