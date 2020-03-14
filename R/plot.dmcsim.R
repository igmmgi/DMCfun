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
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param ylimRt ylimit for rt plot
#' @param ylimErr ylimit for er plot
#' @param legend TRUE/FALSE (or FUNCTION) plot legend on each plot
#' @param labels Condition labels c("Compatible", "Incompatible") default
#' @param cols Condition colours c("green", "red") default
#' @param errorBars TRUE/FALSE
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
#' dmc = dmcSim(fullData = TRUE, mar = c(4,4,3,3))
#' plot(dmc, mar = c(4,4,3,3))
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
                        ylimCAF = c(0, 1),
                        cafBinLabels = FALSE,
                        ylimDelta = c(-50, 150),
                        ylimRt = c(200, 800),
                        ylimErr = c(0, 20),
                        legend = TRUE,
                        labels = c("Compatible", "Incompatible"),
                        cols = c("green", "red"),
                        errorBars = TRUE,
                        xlabs = TRUE,
                        ylabs = TRUE,
                        xaxts = TRUE,
                        yaxts = TRUE,
                        resetLayout = TRUE,
                        ...) {

  figTypes <- c("summary1", "summary2", "summary3", "all", "activation", "trials", "pdf", "cdf", "caf", "delta", "rtCorrect", "rtErrors", "errorRate")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if ("summary1" %in% figType  & !("trials" %in% names(x))) {
    figType = "summary2"
  }
  if (figType %in% c("trials", "activation") & !("trials" %in% names(x))) {
    stop("plotting trials/activation data requires dmcSim with fullData = TRUE")
  }
  if (!(is.function(legend)) && !(legend %in% c(TRUE, FALSE))) {
    stop("legend must be TRUE/FALSE or a function")
  }

  showFig <- rep(FALSE, 9)

  # xlabels
  if (xlabs) {
    xlabs           <- rep("", 9)
    xlabs[c(1:4,6)] <- c("Time [ms]")
    xlabs[c(5)]     <- c("RT Bin")
  } else {
    xlabs <- rep("", 9)
  }

  # ylabels
  if (ylabs) {
    ylabs <- c("E[X(t)]", "X(t)", "PDF", "CDF", "CAF", expression(Delta), "RT Correct [ms]", "Error Rate [%]", "RT Error [%]")
  } else {
    ylabs <- rep("", 9)
  }

  # xaxts & yaxts
  xaxts <- ifelse(xaxts, "s", "n")
  yaxts <- ifelse(yaxts, "s", "n")

  if (figType == "summary1") {
    par(mar = c(4, 4, 1, 1), ...)
    layout(matrix(
      c(1, 1, 3, 4,
        1, 1, 3, 4,
        1, 1, 5, 5,
        2, 2, 5, 5,
        2, 2, 6, 6,
        2, 2, 6, 6),
      nrow = 6, ncol = 4, byrow = TRUE))
    showFig[1:6] = TRUE
  } else if (figType == "summary2") {
    par(mar = c(4, 4, 1, 1))
    layout(matrix(
      c(1, 2,
        1, 2,
        3, 3,
        3, 3,
        4, 4,
        4, 4),
      nrow = 6, ncol = 2, byrow = TRUE))
    showFig[3:6] = TRUE
  } else if (figType == "summary3") {
    par(mar = c(4, 4, 2, 2), mfrow=c(3, 1), ...)
    showFig[7:9] = TRUE
  } else if (figType == "all" & length(x) == 6) {
    par(mar = c(4, 4, 2, 2), mfrow=c(1, 1), ...)
    showFig[1:9] = TRUE
  } else if (figType == "all" & length(x) == 5) {
    par(mar = c(4, 4, 2, 2), mfrow=c(1, 1), ...)
    showFig[4:9] = TRUE
  } else {
    showFig[figTypes[5:13] %in% figType] = TRUE
  }

  # activation
  if (showFig[1]) {

    # automatic
    plot(c(1:x$prms$tmax), x$sim$eq4, type = "l", lty = 2, col = cols[1],
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlab = xlabs[1], ylab = ylabs[1],
         xaxt = xaxts, yaxt = yaxts, ...)
    lines(c(1:x$prms$tmax), -x$sim$eq4, type = "l", lty = 2, col = cols[2], ...)

    # controlled
    dr <- ifelse(x$prms$varDR, mean(as.numeric(as.character(x$prms$drLim)[2:3])), x$prms$mu)
    lines(c(1:x$prms$tmax), cumsum(rep(dr, x$prms$tmax)), ...)

    # superimposed automatic + controlled comp/incomp
    lines(c(1:x$prms$tmax), x$sim$activation_comp,   col = cols[1], ...)
    lines(c(1:x$prms$tmax), x$sim$activation_incomp, col = cols[2], ...)

    # bounds
    lines(c(0, x$prms$tmax), c( x$prms$bnds,  x$prms$bnds), type = "l", col = "darkgrey", ...)
    lines(c(0, x$prms$tmax), c(-x$prms$bnds, -x$prms$bnds), type = "l", col = "darkgrey", ...)

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", legend = labels, col = cols, lty = c(1, 1), inset = c(0.05, 0.2))
    }

  }

  # individual trials
  if (showFig[2]) {

    # bounds
    plot(c(0, x$prms$tmax), c(x$prms$bnds, x$prms$bnds), type = "l", col = "darkgrey",
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlab = xlabs[2], ylab = ylabs[2],
         xaxt = xaxts, yaxt = yaxts, ...)
    lines(c(0, x$prms$tmax), c(-x$prms$bnds, -x$prms$bnds), type = "l", col = "darkgrey", ...)

    # individual trials until bounds
    for (trl in c(1:x$prms$nTrlData)) {
      idx <- which(abs(x$trials$comp[[trl]]) >= x$prms$bnds)[1]
      lines(x$trials$comp[[trl]][1:idx], type = "l", col = cols[1], ...)
      idx <- which(abs(x$trials$incomp[[trl]]) >= x$prms$bnds)[1]
      lines(x$trials$incomp[[trl]][1:idx], type = "l", col = cols[2], ...)
    }

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", legend = labels, col = cols, lty = c(1, 1), inset = c(0.05, 0.2))
    }

  }

  # PDF
  if (showFig[3]) {
    plot(density(x$sim$rts_comp), col = cols[1], main = NA, type = "l",
         ylim = c(0, 0.01), xlim = c(0, x$prms$tmax),
         ylab = ylabs[3], xlab = xlabs[3], xaxt = xaxts, yaxt = "n", ...)
    lines(density(x$sim$rts_incomp), col = cols[2], type = "l", ...)
    if (yaxts == "s") {
      axis(2, at = c(0, 0.005, 0.01), labels = c("0", ".005", ".001"))
    }

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("topright", legend = labels, col = cols, lty = c(1, 1), inset = c(0.05, 0.05))
    }

  }

  # CDF
  if (showFig[4]) {
    density_comp   <- density(x$sim$rts_comp)
    cdf_comp       <- cumsum(density_comp$y * diff(density_comp$x[1:2]))
    density_incomp <- density(x$sim$rts_incomp)
    cdf_incomp     <- cumsum(density_incomp$y * diff(density_incomp$x[1:2]))
    plot(density_comp$x, cdf_comp, type="l", col = cols[1],
         ylab = ylabs[4], xlab = xlabs[4],
         ylim = c(0, 1.5), xaxt = xaxts, yaxt = "n", xlim = c(0, x$prms$tmax))
    lines(density_incomp$x, cdf_incomp, type = "l", col = cols[2])
    abline(h = 0, lty = 2)
    abline(h = 1, lty = 2)
    if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.5), labels = as.character(seq(0, 1, 0.5)))
    }

    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("topright", legend = labels, col = cols, lty = c(1, 1), inset = c(0.05, 0.05))
    }

  }

  # CAF
  if (showFig[5]) {
    plot(x$caf$accPer[x$caf$Comp == "comp"], type = "o",
         ylim = ylimCAF,
         ylab = ylabs[5],  xlab = xlabs[5],
         xaxt = xaxts,  yaxt = "n",
         col = cols[1], ...)
    lines(x$caf$accPer[x$caf$Comp == "incomp"], col = cols[2], type = "o", ...)
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
      legend("bottomright", legend = labels, col = cols, lty = c(1, 1), inset = c(0.05, 0.05))
    }

  }

  # delta
  if (showFig[6]) {
    plot(x$delta$meanBin, x$delta$meanEffect, type = "o",
         ylim = ylimDelta, xlim = c(0, x$prms$tmax),
         ylab = ylabs[6],  xlab = xlabs[6],
         xaxt = xaxts, yaxt = yaxts, ...)
  }

  # rtCorrect
  if (showFig[7]) {
    plot(c(1, 2), x$means$rtCor, type = "o",
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[7], xlab = xlabs[7],
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = labels)
    if (errorBars) {
      addErrorBars(c(1, 2), x$means$rtCor, x$means$sdRtCor)
    }
  }

  # error rate
  if (showFig[8]) {
    plot(c(1, 2), x$means$perErr, type = "o",
         ylim = ylimErr, xlim = c(0.5, 2.5),
         ylab = ylabs[8], xlab = xlabs[8],
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = labels)
  }

  # rtError
  if (showFig[9]) {
    plot(c(1, 2), x$means$rtErr, type = "o",
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[9], xlab = xlabs[9],
         xaxt = "n",  yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = labels)
    if (errorBars) {
      addErrorBars(c(1, 2), x$means$rtErr, x$means$sdRtErr)
    }
  }

  # reset par
  if (resetLayout) {
    par(mfrow=c(1, 1))
  }

}
