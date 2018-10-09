#' @title plot.dmcob
#'
#' @description Plot results from the output of dmcSim. The plot
#' can be an overall summary, or individual plots (activation, trials,
#' pdf, cdf, caf, delta, all). Plot type summary1 contains an activation plot,
#' example individual trials, the probaility distribution function (PDF), the
#' cumulative distribution function (CDF), the conditional accuracy functin (CAF)
#' and delta plots. This required that dmcSim is run with fullData = TRUE. Plot
#' type summary2 contains only the PDF, CDF, CAF and delta plots and does not require
#' that dmcSim is run with fullData = TRUE.
#'
#' @param x Output from fitDMC
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
#' @param newFig TRUE/FALSE
#' @param VP NULL (aggregated data across all participants) or integer for participant number
#' @param errorBars TRUE(default)/FALSE Plot errorbars
#' @param errorBarType sd(default), or se, or ...
#' @param ... pars
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1 (real dataset)
#' plot(flankerData1)
#' plot(flankerData1, errorBarType = "se")
#' plot(flankerData1, figType = "delta", errorBarType = "sd")
#'
#' # Example 2 (real dataset)
#' plot(simonData1)
#' plot(simonData1, errorBarType = "se")
#' plot(simonData1, figType = "delta", errorBarType = "sd")
#'
#' # Example 3 (simulated dataset)
#' dat <- createDF(nVP = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp"), vals = c(420, 100, 80)),
#'                            list(c("Comp:incomp"), vals = c(470, 100, 95))),
#'                  Error = list(list(c("Comp:comp"), vals = c(5, 3, 2, 1, 2)),
#'                          list(c("Comp:incomp"), vals = c(15, 8, 4, 2, 2))))
#' datOb <- dmcObservedData(dat)
#' plot(datOb, errorBarType = "sd")
#'
#' # Example 4 (simulated dataset)
#' dat <- createDF(nVP = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp"), vals = c(420, 100, 150)),
#'                            list(c("Comp:incomp"), vals = c(470, 100, 120))),
#'                  Error = list(list(c("Comp:comp"), vals = c(5, 3, 2, 1)),
#'                          list(c("Comp:incomp"), vals = c(15, 8, 4, 2))))
#' datOb <- dmcObservedData(dat, stepCAF = 25)
#' plot(datOb)
#' plot(datOb, VP = 1)
#' }
#'
#' @export
plot.dmcob <- function(x,
                       figType = "summary",
                       newFig = TRUE,
                       VP = NULL,
                       errorBars = FALSE,
                       errorBarType = "sd",
                       ...) {

  resetFig = FALSE
  if (newFig) {
    par(mfrow = c(1, 1))
  }

  if (is.null(VP)) {
    x$summary <- x$summaryAgg
    x$delta   <- x$deltaAgg
    x$caf     <- x$cafAgg
  } else {
    if (!VP %in% x$summaryVP$VP) {
      stop("datOb does not contain requested VP!")
    }
    x$summary <- x$summaryVP[x$summaryVP$VP == VP, ]
    x$delta   <- x$deltaVP[x$deltaVP$VP == VP, ]
    x$caf     <- x$cafVP[x$cafVP$VP == VP, ]
    errorBars <- FALSE
  }

  if (!is.null(errorBars)) {
    if ((!is.character(errorBarType)) | (!errorBarType %in% c("sd", "se"))) {
      stop("errorBar must be either \"sd\", or \"se\"!")
    }
  }

  if (figType == "summary") {

    if (newFig) {
      par(mar = c(4, 4, 2, 2), cex.axis = 1, cex.lab = 1)
      layout(matrix(c(1, 4,
                      2, 5,
                      3, 6),
                    nrow = 3, ncol = 2, byrow = TRUE))
    }

    plot(x, figType = "rtCorrect", newFig = FALSE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "errorRate", newFig = FALSE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "rtErrors",  newFig = FALSE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "cdf",       newFig = FALSE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "caf",       newFig = FALSE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "delta",     newFig = FALSE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)

    resetFig <- TRUE

  } else if (figType == "rtCorrect") {

    plot(c(1, 2), x$summary$rtCor, type = "b",
         ylim = c(200, 800), xlim = c(0.5, 2.5),
         ylab = "RT Correct [ms]", xlab = "", xaxt = "n", cex = 1)
    axis(1, at = c(1, 2), labels = c("Comp", "Incomp"))

    if (errorBars) {
      addErrorBars(c(1,2), x$summary$rtCor, x$summary[[paste0(errorBarType, "RtCor")]])
    }

  } else if (figType == "errorRate") {

    plot(c(1, 2), x$summary$perErr, type = "b",
         ylim = c(0, 20), xlim = c(0.5, 2.5),
         ylab = "Error Rate [%]", xlab = "", xaxt = "n", cex = 1)
    axis(1, at = c(1, 2), labels = c("Comp", "Incomp"))

    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$perErr, x$summary[[paste0(errorBarType, "PerErr")]])
    }

  } else if (figType == "rtErrors") {

    plot(c(1, 2), x$summary$rtErr, type = "b",
         ylim = c(200, 800), xlim = c(0.5, 2.5),
         ylab = "RT Error [ms]", xlab = "", xaxt = "n", cex = 1)
    axis(1, at = c(1, 2), labels = c("Comp", "Incomp"))

    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$rtErr, x$summary[[paste0(errorBarType, "RtErr")]])
    }

  } else if (figType == "cdf") {

    seqStep <- 100 / (nrow(x$delta) + 1)

    plot(x$delta$meanComp, seq(seqStep, 100 - seqStep, seqStep)/100,
         type = "b", ylim = c(0, 1), xlim = c(200, 1000),
         col = "green", ylab = "CDF", xlab = "t [ms]", yaxt = "n", cex = 2)
    lines(x$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100,
          type = "b", col = "red", cex = 2)
    legend("bottomright", inset = c(0.025, 0.05),
           legend = c("Comp", "Incomp"),
           lty = c(1, 1), col = c("green", "red"), pch = c(1, 1), cex = 1)
    axis(2, at = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", ".25", ".5", ".75", "1") )

  } else if (figType == "caf") {

    plot(x$caf$accPer[x$caf$Comp == "comp"],  type = "b", col = "green",
         ylab = "CAF", ylim = c(0, 1), yaxt = "n",
         xlab = "RT Bin", xaxt = "n", cex = 2)
    lines(x$caf$accPer[x$caf$Comp == "incomp"],  type = "b", col = "red", cex = 2)
    legend("bottomright", inset = c(0.025, 0.05),
           legend = c("Comp", "Incomp"),
           lty = c(1, 1), col = c("green", "red"), pch = c(1, 1), cex = 1)
    axis(1, at = seq(1, nrow(x$caf)/2, 1))
    axis(2, at = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", ".25", ".5", ".75", "1") )

  } else if (figType == "delta") {

    plot(x$delta$meanBin, x$delta$meanEffect, type = "b",
         ylim = c(-50, 150), xlim = c(200, 1000),
         ylab = expression("Delta"), xlab = "t [ms]", cex = 1)

    if (errorBars) {
      errorBarCol <- which(grepl(errorBarType, colnames(x$delta)))
      addErrorBars(x$delta$meanBin,
                   x$delta$meanEffect,
                   x$delta[[errorBarCol]],
                   arrowSize = 0.05)
    }

  } else if (figType == "all") {

    plot(x, figType = "rtCorrect", newFig = TRUE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "errorRate", newFig = TRUE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "rtErrors",  newFig = TRUE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "cdf",       newFig = TRUE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "caf",       newFig = TRUE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)
    plot(x, figType = "delta",     newFig = TRUE, VP = VP, errorBars = errorBars, errorBarType = errorBarType)

  }

  if (resetFig) {
    par(mfrow = c(1, 1))
  }

}
