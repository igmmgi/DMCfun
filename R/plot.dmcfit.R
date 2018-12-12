#' @title plot.dmcfit
#'
#' @description Plot the simulation results from the output of dmcSim. The plot
#' can be an overall summary, or individual plots (activation, trials, pdf, cdf,
#' caf, delta, all). Plot type summary1 contains an activation plot, example
#' individual trials, the probaility distribution function (PDF), the cumulative
#' distribution function (CDF), the conditional accuracy functin (CAF) and
#' delta plots. This required that dmcSim is run with fullData = TRUE. Plot type
#' summary2 contains only the PDF, CDF, CAF and delta plots and does not require
#' that dmcSim is run with fullData = TRUE.
#'
#' @param x Output from dmcFit
#' @param y Observed data
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
#' @param newFig TRUE/FALSE
#' @param VP NULL (aggregated data across all participants) or integer for participant number
#' @param ... pars
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1
#' resTh <- dmcFitAgg(flankerData1, nTrl = 5000)
#' plot(resTh, flankerData1)
#'
#' # Example 2
#' resTh <- dmcFitAgg(flankerData2, nTrl = 5000)
#' plot(resTh, flankerData2)
#' plot(resTh, flankerData2, figType = "all")
#'
#' # Example 3
#' resTh <- dmcFitAgg(simonData1, nTrl = 5000)
#' plot(resTh, simonData1)
#'
#' # Example 4
#' resTh <- dmcFitAgg(simonData1, nTrl = 5000, VP = 1)
#' plot(simonData1, VP = 1)
#' plot(resTh, simonData1, VP = 1)
#' }
#'
#' @export
plot.dmcfit <- function(x,
                        y,
                        figType = "summary",
                        newFig = TRUE,
                        VP     = NULL,
                        ...) {

  resetFig = FALSE
  if (newFig) {
    par(mfrow = c(1, 1))
  }

  if (is.null(VP)) {
    x$summary <- x[[1]]$summary
    x$delta   <- x[[1]]$delta
    x$caf     <- x[[1]]$caf
    y$summary <- y$summaryAgg
    y$delta   <- y$deltaAgg
    y$caf     <- y$cafAgg
  } else {
    if (!VP %in% y$summaryVP$VP) {
      stop("datOb does not contain requested VP!")
    }
    x$summary <- x[[VP]][[1]]$summary
    x$delta   <- x[[VP]][[1]]$delta
    x$caf     <- x[[VP]][[1]]$caf
    y$summary <- y$summaryVP[y$summaryVP$VP == VP, ]
    y$delta   <- y$deltaVP[y$deltaVP$VP == VP, ]
    y$caf     <- y$cafVP[y$cafVP$VP == VP, ]
  }

  if (figType == "summary") {
    if (newFig) {
      par(mar = c(4, 4, 2, 2), cex.axis = 1, cex.lab = 1)
      layout(matrix(c(1, 4,
                      2, 5,
                      3, 6),
                    nrow = 3, ncol = 2, byrow = TRUE))
    }

    plot(x, y, figType = "rtCorrect", newFig = FALSE, VP = VP)
    plot(x, y, figType = "errorRate", newFig = FALSE, VP = VP)
    plot(x, y, figType = "rtErrors",  newFig = FALSE, VP = VP)
    plot(x, y, figType = "cdf",       newFig = FALSE, VP = VP)
    plot(x, y, figType = "caf",       newFig = FALSE, VP = VP)
    plot(x, y, figType = "delta",     newFig = FALSE, VP = VP)

    resetFig <- TRUE

  } else if (figType == "rtCorrect") {

    plot(y$summary$rtCor, type = "b",
         ylim = c(200, 800), ylab = "RT Correct [ms]",
         xlim = c(0.5, 2.5), xlab = "", xaxt = "n", cex = 1)

    lines(c(x[[1]]$summary$rtCor), type = "b", lty = 2, cex = 1)
    axis(1, at = c(1, 2), labels = c("Comp", "Incomp"))
    legend("bottomleft", inset = c(0.025, 0.05),
           legend = c("Observed", "Predicted"),
           lty = c(1, 2), pch = c(1, 1), cex = 1)

  } else if (figType == "errorRate") {

    plot(y$summary$perErr, type = "b",
         ylim = c(0, 20), ylab = "Error Rate [%]",
         xlim = c(0.5, 2.5), xlab = "", xaxt = "n", cex = 1)
    lines(x[[1]]$summary$perErr,
          type = "b", lty = 2, cex = 1)
    axis(1, at = c(1, 2), labels = c("Comp", "Incomp"))
    legend("topleft", inset = c(0.025, 0.05),
           legend = c("Observed", "Predicted"),
           lty = c(1, 2), pch = c(1, 1), cex = 1)

  } else if (figType == "rtErrors") {

    plot(y$summary$rtErr, type = "b",
         ylim = c(200, 800), ylab = "RT Error [ms]",
         xlim = c(0.5, 2.5), xlab = "", xaxt = "n", cex = 1)
    lines(x[[1]]$summary$rtErr,
          type = "b", lty = 2, cex = 1)
    axis(1, at = c(1, 2), labels = c("Comp", "Incomp"))
    legend("bottomleft", inset = c(0.025, 0.05),
           legend = c("Observed", "Predicted"),
           lty = c(1, 2), pch = c(1, 1), cex = 1)

  } else if (figType == "cdf") {

    seqStep <- 100 / (nrow(y$delta) + 1)

    plot(y$delta$meanComp, seq(seqStep, 100 - seqStep, seqStep)/100,
         type = "p", col = "green",
         ylim = c(0, 1), ylab = "CDF", yaxt = "n",
         xlim = c(200, 1000), xlab = "t [ms]", cex = 2)
    lines(y$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100,
          type = "p", col = "red", cex = 2)
    lines(x$delta$meanComp, seq(seqStep, 100 - seqStep, seqStep)/100,
          type = "l", col = "green")
    lines(x$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100,
          type = "l", col = "red")
    legend("bottomright", inset = c(0.025, 0.05),
           legend = c("Comp Observed", "Incomp Observed",
                      "Comp Predicted", "Incomp Predicted"),
           lty = c(0, 0, 1, 1), col = c("green", "red", "green", "red"), pch = c(1, 1, NA, NA), cex = 1)
    axis(2, at = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", ".25", ".5", ".75", "1") )

  } else if (figType == "caf") {

    plot(y$caf$accPer[y$caf$Comp == "comp"],
         type = "p", col = "green",
         ylab = "CAF", ylim = c(0, 1), yaxt = "n",
         xlab = "RT Bin", cex = 2, xaxt = "n")
    lines(y$caf$accPer[y$caf$Comp == "incomp"],
          type = "p", col = "red", cex = 2)
    lines(x$caf$accPer[x$caf$Comp == "comp"],
          type = "l", col = "green")
    lines(x$caf$accPer[x$caf$Comp == "incomp"],
          type = "l", col = "red")

    legend("bottomright", inset = c(0.025, 0.05),
           legend = c("Comp Observed", "Incomp Observed",
                      "Comp Predicted", "Incomp Predicted"),
           lty = c(0, 0, 1, 1), col = c("green", "red", "green", "red"), pch = c(1, 1, NA, NA), cex = 1)
    axis(1, at = seq(1, nrow(x$caf)/2, 1))
    axis(2, at = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", ".25", ".5", ".75", "1") )

  } else if (figType == "delta") {

    plot(y$delta$meanBin, y$delta$meanEffect,
         ylim = c(-50, 150), xlim = c(200, 1000),
         ylab = expression("Delta"), xlab = "t [ms]", cex = 1)
    lines(x$delta$meanBin, x$delta$meanEffect, cex = 1)
    legend("bottomright", inset = c(0.025, 0.05),
           legend = c("Observed", "Predicted"),
           lty = c(0, 1), pch = c(1, NA), cex = 1)

  } else if (figType == "all") {

    plot(x, y, figType = "rtCorrect", newFig = TRUE, VP = VP)
    plot(x, y, figType = "errorRate", newFig = TRUE, VP = VP)
    plot(x, y, figType = "rtErrors",  newFig = TRUE, VP = VP)
    plot(x, y, figType = "cdf",       newFig = TRUE, VP = VP)
    plot(x, y, figType = "caf",       newFig = TRUE, VP = VP)
    plot(x, y, figType = "delta",     newFig = TRUE, VP = VP)

  }

  if (resetFig) {
    par(mfrow = c(1, 1))
  }

}
