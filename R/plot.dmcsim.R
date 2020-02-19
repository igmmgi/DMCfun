#' @title plot.dmcsim
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
#' @param x Output from dmcSim
#' @param figType summary1, summary2, summary3, activation, trials, pdf, cdf, caf, delta, rtCorrect, rtErrors, errorRate, all
#' @param newFig TRUE/FALSE
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param ylimRt ylimit for rt plot
#' @param ylimErr ylimit for er plot
#' @param errorBars TRUE/FALSE
#' @param ... additional plot pars
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1
#' dmc = dmcSim(fullData = TRUE, stepCAF = 10)
#' plot(dmc)
#'
#' # Example 2
#' dmc = dmcSim()
#' plot(dmc)
#'
#' # Example 3
#' dmc = dmcSim()
#' plot(dmc, figType = "all")
#'
#' # Example 4
#' dmc = dmcSim()
#' plot(dmc, figType = "summary3")
#
#' }
#'
#' @export
plot.dmcsim <- function(x,
                        figType = "summary1",
                        newFig = TRUE,
                        ylimCAF = c(0, 1),
                        cafBinLabels = FALSE,
                        ylimDelta = c(-50, 150),
                        ylimRt = c(200, 800),
                        ylimErr = c(0, 20),
                        errorBars = TRUE,
                        ...) {

  if (!figType %in% c("summary1", "summary2", "summary3",  "activation", "trials", "pdf", "cdf", "caf", "delta", "rtCorrect", "rtErrors", "errorRate", "all")) {
    stop("figType must be one of \"summary1\", \"summary2\", \"summary3\", \"activation\", \"trials\", \"pdf\", \"cdf\", \"caf\", \"delta\", \"rtCorrect\", \"rtErrors\", \"errorRate\",  or \"all\"!")
  }

  if (figType == "summary1" & !("trials" %in% names(x))) {
    figType = "summary2"
  }

  if (figType %in% c("trials", "activation") & !("trials" %in% names(x))) {
    stop("plotting trials/activation data requires dmcSim with fullData = TRUE")
  }

  resetFig = FALSE
  if (newFig) {
    par(mfrow = c(1, 1))
  }

  if (figType == "summary1") {
    if (newFig) {
      par(mar = c(4, 4, 2, 2))
      layout(matrix(
        c(1, 1, 3, 4,
          1, 1, 3, 4,
          1, 1, 5, 5,
          2, 2, 5, 5,
          2, 2, 6, 6,
          2, 2, 6, 6),
        nrow = 6, ncol = 4, byrow = TRUE))
    }

    plot(x, figType = "activation", newFig = FALSE, ...)
    plot(x, figType = "trials",     newFig = FALSE, ...)
    plot(x, figType = "pdf",        newFig = FALSE, ...)
    plot(x, figType = "cdf",        newFig = FALSE, ...)
    plot(x, figType = "caf",        newFig = FALSE, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ...)
    plot(x, figType = "delta",      newFig = FALSE, ylimDelta = ylimDelta, ...)

    resetFig <- TRUE

  } else if (figType == "summary2") {

    if (newFig) {
      par(mar = c(4, 4, 2, 2))
      layout(matrix(
        c(1, 2,
          1, 2,
          3, 3,
          3, 3,
          4, 4,
          4, 4),
        nrow = 6, ncol = 2, byrow = TRUE))
    }

    plot(x, figType = "pdf",   newFig = FALSE, ...)
    plot(x, figType = "cdf",   newFig = FALSE, ...)
    plot(x, figType = "caf",   newFig = FALSE, ylimCAF = ylimCAF, ...)
    plot(x, figType = "delta", newFig = FALSE, ylimDelta = ylimDelta, ...)

    resetFig <- TRUE

  } else if (figType == "summary3") {

    if (newFig) {
      par(mar = c(4, 4, 2, 2), mfrow=c(3, 1))
    }
    plot(x, figType = "rtCorrect", newFig = FALSE, ylimRt = ylimRt, errorBars = errorBars, ...)
    plot(x, figType = "rtErrors",  newFig = FALSE, ylimRt = ylimRt, errorBars = errorBars, ...)
    plot(x, figType = "errorRate", newFig = FALSE, ylimErr = ylimErr, ...)

    resetFig <- TRUE

  } else if (figType == "activation") {

    plot(c(1:x$prms$tmax), x$sim$eq4, type = "l", lty = 2, col = "green",
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlab = "", ylab = "E[X(t)]", ...)
    lines(c(1:x$prms$tmax), -x$sim$eq4, type = "l", lty = 2, col = "red", ...)
    
    if (x$prms$varDR) {
      dr <- mean(as.numeric(as.character(x$prms$drLim)[2:3]))
    } else {
      dr <- x$prms$mu
    }
    
    lines(c(1:x$prms$tmax), cumsum(rep(dr, x$prms$tmax)), ...)
    lines(c(1:x$prms$tmax), x$sim$activation_comp,   col = "green", ...)
    lines(c(1:x$prms$tmax), x$sim$activation_incomp, col = "red", ...)
    legend("bottomright", legend = c("Compatible", "Incompatible"),
           col = c("green", "red"), lty = c(1, 1), inset = c(0.05, 0.05))

  } else if (figType == "trials") {

    plot(c(0, x$prms$tmax), c(x$prms$bnds, x$prms$bnds), type = "l",
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlab = "Time [ms]", ylab = "X(t)", ...)
    lines(c(0, x$prms$tmax), c(-x$prms$bnds, -x$prms$bnds), type = "l", ...)

    for (trl in c(1:x$prms$nTrlData)) {
      idx <- which(abs(x$trials$comp[[trl]]) >= x$prms$bnds)[1]
      lines(x$trials$comp[[trl]][1:idx], type = "l", col = "green", ...)
      idx <- which(abs(x$trials$incomp[[trl]]) >= x$prms$bnds)[1]
      lines(x$trials$incomp[[trl]][1:idx], type = "l", col = "red", ...)
    }

    legend("bottomright", legend = c("Compatible", "Incompatible"), col = c("green", "red"),
           lty = c(1, 1), inset = c(0.05, 0.2))

  } else if (figType == "pdf") {

    plot(density(x$sim$rts_comp), col = "green", main = NA, type = "l",
         ylim = c(0, 0.01), xlim = c(0, x$prms$tmax),
         ylab = "PDF", xlab = "Time [ms]",
         yaxt = "n", ...)
    lines(density(x$sim$rts_incomp), col = "red", type = "l", ...)
    legend("topright", legend = c("Compatible", "Incompatible"),
           col = c("green", "red"), lty = c(1, 1), inset = c(0.05, 0.05))
    axis(2, at = c(0, 0.005, 0.01), labels = c("0", ".005", ".001"))

  } else if (figType == "cdf") {

    density_comp <- density(x$sim$rts_comp)
    cdf_comp     <- cumsum(density_comp$y * diff(density_comp$x[1:2]))

    density_incomp <- density(x$sim$rts_incomp)
    cdf_incomp     <- cumsum(density_incomp$y * diff(density_incomp$x[1:2]))

    plot(density_comp$x, cdf_comp, type="l", col = "green",
         ylab = "CDF", xlab = "Time [ms]",
         xlim = c(0, x$prms$tmax))
    lines(density_incomp$x, cdf_incomp, type = "l", col = "red")
    abline(h = 0, lty = 2)
    abline(h = 1, lty = 2)

  } else if (figType == "caf") {

    plot(x$caf$accPer[x$caf$Comp == "comp"], type = "o",
         ylim = ylimCAF,
         ylab = "CAF",  xlab = "RT Bin",
         xaxt = "n",  yaxt = "n",
         col = "green", ...)
    lines(x$caf$accPer[x$caf$Comp == "incomp"], col = "red", type = "o", ...)
    legend("bottomright", legend = c("Compatible", "Incompatible"),
           col = c("green", "red"), lty = c(1, 1), inset = c(0.05, 0.05))

    nCAF <- length(x$caf$bin) / 2
    if (cafBinLabels) {
      stepCAF <- 100 / nCAF
      cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
      axis(1, at = seq(1, nCAF, 1), labels = cafLabels)
    } else {
      axis(1, at = seq(1, nCAF, 1))
    }
    axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)))

  } else if (figType == "delta") {

    plot(x$delta$meanBin, x$delta$meanEffect, type = "o",
         ylim = ylimDelta, xlim = c(0, x$prms$tmax),
         ylab = expression(Delta),   xlab = "Time [ms]", ...)

  } else if (figType == "rtCorrect") {

    plot(c(1, 2), x$means$rtCor, type = "o",
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = "RT Correct [ms]", xlab = "",
         xaxt = "n", ...)
    axis(1, at = c(1, 2), labels = c("Compatible", "Incompatible"))

    if (errorBars) {
      addErrorBars(c(1, 2), x$means$rtCor, x$means$sdRtCor)
    }

  } else if (figType == "errorRate") {

    plot(c(1, 2), x$means$perErr, type = "o",
         ylim = ylimErr, xlim = c(0.5, 2.5),
         ylab = "Error Rate [%]", xlab = "",
         xaxt = "n", ...)
    axis(1, at = c(1, 2), labels = c("Compatible", "Incompatible"))

  } else if (figType == "rtErrors") {

    plot(c(1, 2), x$means$rtErr, type = "o",
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = "RT Error [ms]", xlab = "",
         xaxt = "n", ...)
    axis(1, at = c(1, 2), labels = c("Compatible", "Incompatible"))

    if (errorBars) {
      addErrorBars(c(1, 2), x$means$rtErr, x$means$sdRtErr)
    }

  } else if (figType == "all" & length(x) == 6) {

    plot(x, figType = "activation", newFig = TRUE, ...)
    plot(x, figType = "trials",     newFig = TRUE, ...)
    plot(x, figType = "pdf",        newFig = TRUE, ...)
    plot(x, figType = "cdf",        newFig = TRUE, ...)
    plot(x, figType = "caf",        newFig = TRUE, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ...)
    plot(x, figType = "delta",      newFig = TRUE, ylimDelta = ylimDelta, ...)
    plot(x, figType = "rtCorrect",  newFig = TRUE, ylimRt = ylimRt, errorBars = errorBars, ...)
    plot(x, figType = "rtErrors",   newFig = TRUE, ylimRt = ylimRt, errorBars = errorBars, ...)
    plot(x, figType = "errorRate",  newFig = TRUE, ylimErr = ylimErr, ...)

  } else if (figType == "all" & length(x) == 5) {

    plot(x, figType = "pdf",       newFig = TRUE, ...)
    plot(x, figType = "cdf",       newFig = TRUE, ...)
    plot(x, figType = "caf",       newFig = TRUE, cafBinLabels = cafBinLabels, ...)
    plot(x, figType = "delta",     newFig = TRUE, ...)
    plot(x, figType = "rtCorrect", newFig = TRUE, ylimRt = ylimRt, errorBars = errorBars, ...)
    plot(x, figType = "rtErrors",  newFig = TRUE, ylimRt = ylimRt, errorBars = errorBars, ...)
    plot(x, figType = "errorRate", newFig = TRUE, ylimErr = ylimErr, ...)

  }

  if (resetFig) {
    par(mfrow = c(1, 1))
  }

}

