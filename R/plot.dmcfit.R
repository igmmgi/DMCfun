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
#' @param VP NULL (aggregated data across all participants) or integer for participant number
#' @param legend TRUE/FALSE (or FUNCTION) plot legend on each plot
#' @param labels Condition labels c("Compatible", "Incompatible", "Observed", "Predicted") default
#' @param cols Condition colours c("green", "red") default
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
#' # Example 1
#' resTh <- dmcFitAgg(flankerData, nTrl = 5000)
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
                        VP     = NULL,
                        legend = TRUE,
                        labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
                        cols = c("black", "green", "red"),
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
  if (length(labels) != 4) {
    stop("labels must be length 4")
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

  showFig = rep(FALSE, 6)

  # xlabels
  if (xlabs) {
    xlabs          <- rep("", 6)
    xlabs[c(1, 2)] <- c(labels[1], labels[2])
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
    plot(c(1,2), y$summary$rtCor, type = "o", col = cols[1],
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[1], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    lines(c(x$means$rtCor), type = "o", lty = 2, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("topleft", inset = c(0.025, 0.05), legend = labels[3:4], lty = c(1, 2), pch = c(1, 1))
    }

  }

  # errorRate
  if (showFig[2]) {
    plot(c(1,2), y$summary$perErr, type = "o", col = cols[1],
         ylim = ylimEr, xlim = c(0.5, 2.5),
         ylab = ylabs[2], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    lines(x$means$perErr, type = "b", lty = 2, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("topleft", inset = c(0.025, 0.05), legend = labels[3:4], lty = c(1, 2), pch = c(1, 1))
    }

  }

  # rt Error
  if (showFig[3]) {
    plot(c(1,2), y$summary$rtErr, type = "o", col = cols[1],
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[3], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    lines(x$means$rtErr, type = "b", lty = 2, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("topleft", inset = c(0.025, 0.05), legend = labels[3:4], lty = c(1, 2), pch = c(1, 1))
    }

  }

  # cdf
  if (showFig[4]) {
    seqStep <- 100 / (nrow(y$delta) + 1)
    plot(y$delta$meanComp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "p",
         ylim = c(0, 1), xlim = c(200, 1000),
         ylab = ylabs[4], xlab = xlabs[4],
         yaxt = "n", col = tail(cols, 2)[1],
         xaxt = xaxts, yaxt = "n", ...)
    if (xaxts == "n") axis(side=1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") axis(side=2, labels = FALSE)  # keep tick marks
    
    lines(y$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "p", col = tail(cols, 2)[2], ...)
    lines(x$delta$meanComp,   seq(seqStep, 100 - seqStep, seqStep)/100, type = "l", col = tail(cols, 2)[1], ...)
    lines(x$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "l", col = tail(cols, 2)[2], ...)
    if (yaxts ==  "s") {
      axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    }

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05),
             legend = c(paste(labels[1], labels[3], sep = " "),
                        paste(labels[2], labels[3], sep = " "),
                        paste(labels[1], labels[4], sep = " "),
                        paste(labels[2], labels[4], sep = " ")),
             lty = c(0, 0, 1, 1), col = tail(cols, 2), pch = c(1, 1, NA, NA))
    }

  }

  # caf
  if (showFig[5]) {
    plot(y$caf$accPer[y$caf$Comp == "comp"], type = "p",
         ylim = ylimCAF,
         ylab = ylabs[5], xlab = xlabs[5],
         yaxt = "n", xaxt = "n",
         col = tail(cols, 2)[1], ...)
    if (xaxts == "n") axis(side=1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") axis(side=2, labels = FALSE)  # keep tick marks
    
    lines(y$caf$accPer[y$caf$Comp == "incomp"], type = "p", col = tail(cols, 2)[2], ...)
    lines(x$caf$accPer[x$caf$Comp == "comp"],   type = "l", col = tail(cols, 2)[1], ...)
    lines(x$caf$accPer[x$caf$Comp == "incomp"], type = "l", col = tail(cols, 2)[2], ...)
    if (xaxts == "s") {
      nCAF <- length(x$caf$bin) / 2
      if (cafBinLabels) {
        stepCAF <- 100 / nCAF
        cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
        axis(1, at = seq(1, nCAF, 1), labels = cafLabels)
      } else {
        axis(1, at = seq(1, nCAF, 1))
      }
    } else {
      axis(side=1,labels=F) 
    }
    if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)))
    }

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05),
             legend = c(paste(labels[1], labels[3], sep = " "),
                        paste(labels[2], labels[3], sep = " "),
                        paste(labels[1], labels[4], sep = " "),
                        paste(labels[2], labels[4], sep = " ")),
             lty = c(0, 0, 1, 1), col = tail(cols, 2), pch = c(1, 1, NA, NA))
    }
  }

  # delta
  if (showFig[6]) {
    plot(y$delta$meanBin, y$delta$meanEffect, col = cols[1],
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = ylabs[6], xlab = xlabs[6],
         xaxt = xaxts, yaxt = yaxts, ...)
    lines(x$delta$meanBin, x$delta$meanEffect, ...)
    axis(side=1, labels = FALSE) 
    axis(side=2, labels = FALSE) 

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05), legend = labels[3:4], lty = c(0, 1), pch = c(1, NA))
    }

  }

  # reset par
  if (resetLayout) {
    par(mfrow=c(1, 1))
  }

}
