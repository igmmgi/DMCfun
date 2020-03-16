#' @title plot.dmcob
#'
#' @description Plot results from the output of dmcSim. The plot
#' can be an overall summary, or individual plots (activation, trials,
#' pdf, cdf, caf, delta, all). Plot type summary1 contains an activation plot,
#' example individual trials, the probability distribution function (PDF), the
#' cumulative distribution function (CDF), the conditional accuracy function (CAF)
#' and delta plots. This required that dmcSim is run with fullData = TRUE. Plot
#' type summary2 contains only the PDF, CDF, CAF and delta plots and does not require
#' that dmcSim is run with fullData = TRUE.
#'
#' @param x Output from fitDMC
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
#' @param VP NULL (aggregated data across all participants) or integer for participant number
#' @param legend TRUE/FALSE (or FUNCTION) plot legend on each plot
#' @param labels Condition labels c("Compatible", "Incompatible") default
#' @param cols Condition colours c("green", "red") default
#' @param errorBars TRUE(default)/FALSE Plot errorbars
#' @param errorBarType sd(default), or se
#' @param ylimRt ylimit for Rt plots
#' @param ylimEr ylimit for error rate plots
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param xlimDelta xlimit for delta plot
#' @param xlabs TRUE/FALSE
#' @param ylabs TRUE/FALSE
#' @param xaxts TRUE/FALSE
#' @param yaxts TRUE/FALSE
#' @param resetLayout TRUE (default)/FALSE Set to FALSE if using custom par(mfrow=c()) before calling plot
#' @param ... additional plot pars
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1 (real dataset)
#' plot(flankerData)
#' plot(flankerData, errorBars = TRUE, errorBarType = "se")
#' plot(flankerData, figType = "delta")
#' plot(flankerData, figType = "caf")
#'
#' # Example 2 (real dataset)
#' plot(simonData)
#' plot(simonData, errorBars = TRUE, errorBarType = "se")
#' plot(simonData, figType = "delta", errorBars = TRUE, errorBarType = "sd")
#'
#' # Example 3 (simulated dataset)
#' dat <- createDF(nVP = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(420, 100, 80),
#'                            "Comp_incomp" = c(470, 100, 95)),
#'                  Error = list("Comp_comp"   = c(5, 3, 2, 1, 2),
#'                               "Comp_incomp" = c(15, 8, 4, 2, 2)))
#' datOb <- dmcObservedData(dat)
#' plot(datOb, errorBars = TRUE, errorBarType = "sd")
#'
#' # Example 4 (simulated dataset)
#' dat <- createDF(nVP = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(420, 100, 150),
#'                            "Comp_incomp" = c(470, 100, 120)),
#'                  Error = list("Comp_comp"   = c(5, 3, 2, 1),
#'                               "Comp_incomp" = c(15, 8, 4, 2)))
#' datOb <- dmcObservedData(dat, stepCAF = 25)
#' plot(datOb)
#' }
#'
#' @export
plot.dmcob <- function(x,
                       figType = "summary",
                       VP = NULL,
                       legend = TRUE,
                       labels = c("Compatible", "Incompatible"),
                       cols = c("black", "green", "red"),
                       errorBars = FALSE,
                       errorBarType = "sd",
                       ylimRt = c(200, 800),
                       ylimEr = c(0, 20),
                       ylimCAF = c(0, 1),
                       cafBinLabels = FALSE,
                       ylimDelta = c(-50, 100),
                       xlimDelta = c(200, 1000),
                       xlabs = TRUE,
                       ylabs = TRUE,
                       xaxts = TRUE,
                       yaxts = TRUE,
                       resetLayout = TRUE,
                       ...) {

  figTypes <- c("summary", "all", "rtCorrect", "errorRate", "rtErrors", "cdf", "caf", "delta")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if (!(is.function(legend)) && !(legend %in% c(TRUE, FALSE))) {
    stop("legend must be TRUE/FALSE or a function")
  }

  if (!is.null(VP)) {
    # select individual dataset
    if (!VP %in% x$summaryVP$VP) {
      stop("datOb does not contain requested VP!")
    }
    x$summary <- x$summaryVP[x$summaryVP$VP == VP, ]
    x$delta   <- x$deltaVP[x$deltaVP$VP == VP, ]
    x$caf     <- x$cafVP[x$cafVP$VP == VP, ]
    errorBars <- FALSE
  }

  if (errorBars) {
    if ((!is.character(errorBarType)) | (!errorBarType %in% c("sd", "se"))) {
      stop("errorBar must be either \"sd\", or \"se\"!")
    }
  }

  showFig = rep(FALSE, 6)

  # xlabels
  if (xlabs) {
    xlabs          <- rep("", 6)
    xlabs[c(4, 6)] <- c("Time [ms]")
    xlabs[c(5)]    <- c("RT Bin")
  } else {
    xlabs <- rep("", 6)
  }

  # ylabels
  if (ylabs) {
    ylabs <- c("RT Correct [ms]", "Error Rate [%]", "RT Error [ms]", "CDF", "CAF", expression(Delta))
  } else {
    ylabs <- rep("", 6)
  }

  # xaxts & yaxts
  xaxts <- ifelse(xaxts, "s", "n")
  yaxts <- ifelse(yaxts, "s", "n")

  if (figType == "summary") {
    par(mar = c(4, 4, 1, 1), ...)
    layout(matrix(c(1, 4,
                    2, 5,
                    3, 6),
                  nrow = 3, ncol = 2, byrow = TRUE))
    showFig[1:6] = TRUE
  } else if (figType == "all") {
    par(mar = c(4, 4, 1, 1), mfrow=c(1, 1), ...)
    showFig[1:6] = TRUE
  } else {
    showFig[figTypes[3:8] %in% figType] = TRUE
  }

  # rtCorrect
  if (showFig[1]) {
    plot(c(1, 2), x$summary$rtCor, type = "o", col = cols[1],
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[1], xlab = xlabs[1],
         xaxt = "n", yaxt = yaxts, ...)
    if (xaxts == "s") {
      axis(1, at = c(1, 2), labels = labels)
    }
    if (errorBars) {
      addErrorBars(c(1,2), x$summary$rtCor, x$summary[[paste0(errorBarType, "RtCor")]])
    }
  }

  # errorRate
  if (showFig[2]) {
    plot(c(1, 2), x$summary$perErr, type = "o", col = cols[1],
         ylim = ylimEr, xlim = c(0.5, 2.5),
         ylab = ylabs[2], xlab = xlabs[2],
         xaxt = "n", yaxt = yaxts, ...)
    if (xaxts == "s") {
      axis(1, at = c(1, 2), labels = labels)
    }
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$perErr, x$summary[[paste0(errorBarType, "PerErr")]])
    }
  }

  # rtError
  if (showFig[3]) {
    plot(c(1, 2), x$summary$rtErr, type = "o", col = cols[1],
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[3], xlab = xlabs[3],
         xaxt = "n", yaxt = yaxts, ...)
    if (xaxts == "s") {
      axis(1, at = c(1, 2), labels = labels)
    }
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$rtErr, x$summary[[paste0(errorBarType, "RtErr")]])
    }
  }

  # CDF
  if (showFig[4]) {
    seqStep <- 100 / (nrow(x$delta) + 1)
    plot(x$delta$meanComp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "o",
         ylim = c(0, 1), xlim = c(200, 1000),
         ylab = ylabs[4], xlab = xlabs[4],
         col = tail(cols, 2)[1],
         xaxt = xaxts, yaxt = "n", ...)
    lines(x$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "o", col = tail(cols, 2)[2], ...)
    if (yaxts ==  "s") {
      axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    }

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05), legend = labels, lty = c(1, 1), col = tail(cols, 2), pch = c(1, 1))
    }

  }

  # caf
  if (showFig[5]) {
    plot(x$caf$accPer[x$caf$Comp == "comp"],  type = "o",
         ylim = ylimCAF,
         ylab = ylabs[5], xlab = xlabs[5],
         col = tail(cols, 2)[1],
         xaxt = "n", yaxt = yaxts, ...)
    lines(x$caf$accPer[x$caf$Comp == "incomp"],  type = "b", col = tail(cols, 2)[2], ...)
    if (xaxts == "s") {
      nCAF <- length(x$caf$bin) / 2
      if (cafBinLabels) {
        stepCAF <- 100 / nCAF
        cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
        axis(1, at = seq(1, nCAF, 1), labels = cafLabels)
      } else {
        axis(1, at = seq(1, nCAF, 1))
      }
    }
    if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)))
    }

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05), legend = labels, lty = c(1, 1), col = tail(cols, 2), pch = c(1, 1))
    }

  }

  if (showFig[6]) {
    plot(x$delta$meanBin, x$delta$meanEffect, type = "o", col = cols[1],
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = ylabs[6], xlab = xlabs[6],
         xaxt = xaxts, yaxt = yaxts, ...)
    if (errorBars) {
      errorBarCol <- which(grepl(errorBarType, colnames(x$delta)))
      addErrorBars(x$delta$meanBin,
                   x$delta$meanEffect,
                   x$delta[[errorBarCol]],
                   arrowSize = 0.05)
    }
  }

  # reset par
  if (resetLayout) {
    par(mfrow=c(1, 1))
  }

}
