#' @title plot.dmcfit
#'
#' @description Plot the simulation results from the output of dmcSim. The plot
#' can be an overall summary, or individual plots (activation, trials, pdf, cdf,
#' caf, delta, all). Plot type summary1 contains an activation plot, example
#' individual trials, the probability distribution function (PDF), the cumulative
#' distribution function (CDF), the conditional accuracy function (CAF) and
#' delta plots. This required that dmcSim is run with fullData = TRUE. Plot type
#' summary2 contains only the PDF, CDF, CAF and delta plots and does not require
#' that dmcSim is run with fullData = TRUE.
#'
#' @param x Output from dmcFit
#' @param y Observed data
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
#' @param newFig TRUE/FALSE
#' @param VP NULL (aggregated data across all participants) or integer for participant number
#' @param ylimRt ylimit for Rt plots
#' @param ylimEr ylimit for error rate plots
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param xlimDelta xlimit for delta plot
#' @param ... additional plot pars
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1
#' resTh <- dmcFitAgg(flankerData, nTrl = 50000)
#' plot(resTh, flankerData)
#'
#' # Example 2
#' resTh <- dmcFitAgg(flankerData, nTrl = 5000)
#' plot(resTh, flankerData)
#' plot(resTh, flankerData, figType = "all")
#'
#' # Example 3
#' resTh <- dmcFitAgg(simonData, nTrl = 5000)
#' plot(resTh, simonData)
#'
#' # Example 4
#' resTh <- dmcFitVPs(simonData, nTrl = 5000, VP = 6)
#' plot(simonData, VP = 6)
#' plot(resTh, simonData, VP = 6)
#' }
#'
#' @export
plot.dmcfit <- function(x,
                        y,
                        figType = "summary",
                        newFig = TRUE,
                        VP     = NULL,
                        ylimRt = c(200, 800),
                        ylimEr = c(0, 20),
                        ylimCAF = c(0, 1),
                        cafBinLabels = FALSE,
                        ylimDelta = c(-50, 100),
                        xlimDelta = c(200, 1000),
                        ...) {

  if (!figType %in% c("summary", "rtCorrect", "errorRate", "rtErrors", "cdf", "caf", "delta", "all")) {
      stop("figType must be one of \"summary\", \"rtCorrect\", \"errorRate\", \"rtErrors\", \"cdf\", \"delta\" or \"all\"!")
  }

  resetFig = FALSE
  if (newFig) {
    par(mfrow = c(1, 1))
  }

  if (is.null(VP)) {
    x$means <- x[[1]]$means
    x$delta <- x[[1]]$delta
    x$caf   <- x[[1]]$caf
  } else {
    if (!VP %in% y$summaryVP$VP) {
      stop("datOb does not contain requested VP!")
    }
    x$means <- x[[VP]][[1]]$means
    x$delta <- x[[VP]][[1]]$delta
    x$caf   <- x[[VP]][[1]]$caf
    y$means <- y$summaryVP[y$summaryVP$VP == VP, ]
    y$delta <- y$deltaVP[y$deltaVP$VP == VP, ]
    y$caf   <- y$cafVP[y$cafVP$VP == VP, ]
  }

  if (figType == "summary") {
    if (newFig) {
      par(mar = c(4, 4, 2, 2))
      layout(matrix(c(1, 4,
                      2, 5,
                      3, 6),
                    nrow = 3, ncol = 2, byrow = TRUE))
    }

    plot(x, y, figType = "rtCorrect", newFig = FALSE, VP = VP, ylimRt = ylimRt, ...)
    plot(x, y, figType = "errorRate", newFig = FALSE, VP = VP, ylimEr = ylimEr, ...)
    plot(x, y, figType = "rtErrors",  newFig = FALSE, VP = VP, ylimRt = ylimRt, ...)
    plot(x, y, figType = "cdf",       newFig = FALSE, VP = VP, ...)
    plot(x, y, figType = "caf",       newFig = FALSE, VP = VP, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ...)
    plot(x, y, figType = "delta",     newFig = FALSE, VP = VP, ylimDelta = ylimDelta, xlimDelta = xlimDelta, ...)

    resetFig <- TRUE

  } else if (figType == "rtCorrect") {

    plot(y$summary$rtCor, type = "o",
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = "RT Correct [ms]", xlab = "", xaxt = "n", ...)
    lines(c(x$means$rtCor), type = "o", lty = 2, ...)
    axis(1, at = c(1, 2), labels = c("Compatible", "Incompatible"))
    legend("topleft", inset = c(0.025, 0.05),
           legend = c("Observed", "Predicted"),
           lty = c(1, 2), pch = c(1, 1))

  } else if (figType == "errorRate") {

    plot(y$summary$perErr, type = "o",
         ylim = ylimEr, xlim = c(0.5, 2.5),
         ylab = "Error Rate [%]", xlab = "", xaxt = "n", ...)
    lines(x$means$perErr, type = "b", lty = 2, ...)
    axis(1, at = c(1, 2), labels = c("Compatible", "Incompatible"))
    legend("topleft", inset = c(0.025, 0.05),
           legend = c("Observed", "Predicted"),
           lty = c(1, 2), pch = c(1, 1))

  } else if (figType == "rtErrors") {

    plot(y$summary$rtErr, type = "o",
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = "RT Error [ms]", xlab = "", xaxt = "n", ...)
    lines(x$means$rtErr, type = "b", lty = 2, ...)
    axis(1, at = c(1, 2), labels = c("Compatible", "Incompatible"))
    legend("topleft", inset = c(0.025, 0.05),
           legend = c("Observed", "Predicted"),
           lty = c(1, 2), pch = c(1, 1))

  } else if (figType == "cdf") {

    seqStep <- 100 / (nrow(y$delta) + 1)

    plot(y$delta$meanComp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "p",
         ylim = c(0, 1), xlim = c(200, 1000),
         ylab = "CDF", xlab = "t [ms]",
         yaxt = "n", col = "green", ...)
    lines(y$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "p", col = "red", ...)
    lines(x$delta$meanComp,   seq(seqStep, 100 - seqStep, seqStep)/100, type = "l", col = "green", ...)
    lines(x$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "l", col = "red", ...)
    legend("bottomright", inset = c(0.025, 0.05),
           legend = c("Compatible Observed", "Incompatible Observed", "Compatible Predicted", "Incompatible Predicted"),
           lty = c(0, 0, 1, 1), col = c("green", "red", "green", "red"), pch = c(1, 1, NA, NA))
    axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))

  } else if (figType == "caf") {

    plot(y$caf$accPer[y$caf$Comp == "comp"], type = "p",
         ylim = ylimCAF,
         ylab = "CAF", xlab = "RT Bin",
         yaxt = "n", xaxt = "n",
         col = "green", ...)
    lines(y$caf$accPer[y$caf$Comp == "incomp"], type = "p", col = "red", ...)
    lines(x$caf$accPer[x$caf$Comp == "comp"],   type = "l", col = "green", ...)
    lines(x$caf$accPer[x$caf$Comp == "incomp"], type = "l", col = "red", ...)

    legend("bottomright", inset = c(0.025, 0.05),
           legend = c("Compatible Observed", "Incompatible Observed", "Compatible Predicted", "Incompatible Predicted"),
           lty = c(0, 0, 1, 1), col = c("green", "red", "green", "red"), pch = c(1, 1, NA, NA))

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

    plot(y$delta$meanBin, y$delta$meanEffect,
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = expression("Delta"), xlab = "t [ms]", ...)
    lines(x$delta$meanBin, x$delta$meanEffect, ...)
    legend("bottomright", inset = c(0.025, 0.05),
           legend = c("Observed", "Predicted"),
           lty = c(0, 1), pch = c(1, NA))

  } else if (figType == "all") {

    plot(x, y, figType = "rtCorrect", newFig = TRUE, VP = VP, ylimRt = ylimRt, ...)
    plot(x, y, figType = "errorRate", newFig = TRUE, VP = VP, ylimEr = ylimEr, ...)
    plot(x, y, figType = "rtErrors",  newFig = TRUE, VP = VP, ylimRt = ylimRt, ...)
    plot(x, y, figType = "cdf",       newFig = TRUE, VP = VP, ...)
    plot(x, y, figType = "caf",       newFig = TRUE, VP = VP, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ...)
    plot(x, y, figType = "delta",     newFig = TRUE, VP = VP, ylimDelta = ylimDelta, xlimDelta = xlimDelta, ...)

  }

  if (resetFig) {
    par(mfrow = c(1, 1))
  }

}
