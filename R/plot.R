#' @title plot.dmcsim: Plot dmc simulation
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
#' @param figType summary1, summary2, summary3, activation, trials, pdf, cdf,
#' caf, delta, rtCorrect, rtErrors, errorRate, all
#' @param xlimActivation xlimit for activation plot
#' @param xlimTrials xlimit for trials plot
#' @param xlimPDF xlimit for PDF plot
#' @param ylimPDF ylimit for PDF plot
#' @param xlimCDF xlimit for CDF plot
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param xlimDelta xlimit for delta plot (Default is 0 to tmax)
#' @param ylimDeltaErrors ylimit for delta plot of errors
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
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' dmc = dmcSim(fullData = TRUE)
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
#'
#' }
#'
#' @export
plot.dmcsim <- function(x,
                        figType = "summary1",
                        xlimActivation = NULL,
                        xlimTrials = NULL,
                        xlimPDF = NULL,
                        ylimPDF = NULL,
                        xlimCDF = NULL,
                        ylimCAF = NULL,
                        cafBinLabels = FALSE,
                        ylimDelta = NULL,
                        xlimDelta = NULL,
                        ylimDeltaErrors = NULL,
                        ylimRt = NULL,
                        ylimErr = NULL,
                        legend = TRUE,
                        labels = c("Compatible", "Incompatible"),
                        cols = c("black", "green", "red"),
                        errorBars = FALSE,
                        xlabs = TRUE,
                        ylabs = TRUE,
                        xaxts = TRUE,
                        yaxts = TRUE,
                        resetPar = TRUE,
                        ...)  {

  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  figType <- tolower(figType)
  figTypes <- c("summary1", "summary2", "summary3", "all", "activation", "trials",
                "pdf", "cdf", "caf", "delta", "deltaerrors", "rtcorrect", "errorrate",  "rterrors")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if ("summary1" %in% figType  & !("trials" %in% names(x))) {
    figType <- "summary2"
  }
  if (figType %in% c("trials", "activation") & !("trials" %in% names(x))) {
    stop("plotting trials/activation data requires dmcSim with fullData = TRUE")
  }
  if (length(labels) != 2) {
    stop("labels must be length 2")
  }
  if (!(is.function(legend)) && !(legend %in% c(TRUE, FALSE))) {
    stop("legend must be TRUE/FALSE or a function")
  }

  showFig <- rep(FALSE, 10)

  # xlabels
  if (xlabs) {
    xlabs            <- rep("", 10)
    xlabs[c(1:4, 6)] <- c("Time [ms]")
    xlabs[c(5, 7)]   <- c("RT Bin")
    xlabs[c(9:10)]   <- c(labels)
  } else {
    xlabs <- rep("", 10)
  }

  # y-labels
  if (ylabs) {
    ylabs <- c("E[X(t)]", "X(t)", "PDF", "CDF", "CAF",
      expression(paste(Delta, " RT [ms]")), expression(paste(Delta, " ER [%]")),
      "RT Correct [ms]", "Error Rate [%]", "RT Error [ms]")
  } else {
    ylabs <- rep("", 10)
  }

  # x-axts and y-axts
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
    showFig[1:6] <- TRUE
  } else if (figType == "summary2") {
    par(mar = c(4, 4, 1, 1), ...)
    layout(matrix(
      c(1, 2,
        1, 2,
        3, 3,
        3, 3,
        4, 4,
        4, 4),
      nrow = 6, ncol = 2, byrow = TRUE))
    showFig[3:6] <- TRUE
  } else if (figType == "summary3") {
    par(mar = c(4, 4, 2, 2), mfrow = c(3, 1), ...)
    showFig[8:10] <- TRUE
  } else if (figType == "all" & length(x) == 7) {
    par(mar = c(4, 4, 2, 2), mfrow = c(1, 1), ...)
    showFig[1:10] <- TRUE
  } else if (figType == "all" & length(x) == 6) {
    par(mar = c(4, 4, 2, 2), mfrow = c(1, 1), ...)
    showFig[4:10] <- TRUE
  } else {
    showFig[figTypes[5:14] %in% figType] <- TRUE
  }

  # activation
  if (showFig[1]) {

    if (is.null(xlimActivation)) {
      xlimActivation <- c(0, x$prms$tmax)
    }

    # automatic
    plot(c(1:x$prms$tmax), x$sim$eq4, type = "l", lty = 2, col = tail(cols, 2)[1],
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlim = xlimActivation,
         xlab = xlabs[1], ylab = ylabs[1],
         xaxt = xaxts, yaxt = yaxts, ...)
    if (xaxts == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") axis(side = 2, labels = FALSE)  # keep tick marks

    lines(c(1:x$prms$tmax), -x$sim$eq4, type = "l", lty = 2, col = tail(cols, 2)[2], ...)

    # controlled
    if (x$prms$drDist != 0) {
      dr <- mean(c(x$prms$drLim1, x$prms$drLim2))
    } else {
      dr <- x$prms$drc
    }
    dr <- cumsum(rep(dr, x$prms$tmax))
    dr <- dr + mean(c(x$prms$spLim1, x$prms$spLim2))
    lines(c(1:x$prms$tmax), dr, ...)

    # superimposed automatic + controlled comp/incomp
    lines(c(1:x$prms$tmax), x$sim$activation_comp,   col = tail(cols, 2)[1], ...)
    lines(c(1:x$prms$tmax), x$sim$activation_incomp, col = tail(cols, 2)[2], ...)

    # bounds
    abline(h = c(-x$prms$bnds, x$prms$bnds), col = "darkgrey");

    add_legend(legend, labels, tail(cols, 2), c(1, 1), c(1, 1))

  }

  # individual trials
  if (showFig[2]) {

    if (is.null(xlimTrials)) {
      xlimTrials <- c(0, x$prms$tmax)
    }

    plot(NULL, NULL,
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlim = xlimTrials,
         xlab = xlabs[2], ylab = ylabs[2],
         xaxt = xaxts, yaxt = yaxts, ...)
    if (xaxts == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") axis(side = 2, labels = FALSE)  # keep tick marks

    # individual trials until bounds
    for (trl in c(1:x$prms$nTrlData)) {
      boundsHit <- abs(x$trials$comp[[trl]]) >= x$prms$bnds
      if (any(boundsHit)) {
        idx <- which(boundsHit)[1]
        lines(x$trials$comp[[trl]][1:idx], type = "l", col = tail(cols, 2)[1], ...)
      }
      boundsHit <- abs(x$trials$incomp[[trl]]) >= x$prms$bnds
      if (any(boundsHit)) {
        idx <- which(boundsHit)[1]
        lines(x$trials$incomp[[trl]][1:idx], type = "l", col = tail(cols, 2)[2], ...)
      }
    }

    # bounds
    abline(h = c(-x$prms$bnds, x$prms$bnds), col = "darkgrey");

    add_legend(legend, labels, tail(cols, 2), c(1, 1), c(1, 1))

  }

  # PDF
  if (showFig[3]) {

    if (is.null(xlimPDF)) {
      xlimPDF <- c(0, x$prms$tmax)
    }
    if (is.null(ylimPDF)) {
      ylimPDF <- c(0, 0.01)
    }

    plot(density(x$sim$rts_comp), col = tail(cols, 2)[1], main = NA, type = "l",
         ylim = ylimPDF, xlim = xlimPDF, ylab = ylabs[3], xlab = xlabs[3],
         xaxt = xaxts, yaxt = "n", ...)

    if (xaxts == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(side = 2, at = c(0, 0.005, 0.01), labels = c("0", ".005", ".001"))
    }

    lines(density(x$sim$rts_incomp), col = tail(cols, 2)[2], type = "l", ...)

    abline(h = 0, col = "darkgrey", lty = 2);
    add_legend(legend, labels, tail(cols, 2), c(1, 1), c(1, 1), position = "topright")

  }

  # CDF
  if (showFig[4]) {

    density_comp   <- density(x$sim$rts_comp)
    cdf_comp       <- cumsum(density_comp$y * diff(density_comp$x[1:2]))
    density_incomp <- density(x$sim$rts_incomp)
    cdf_incomp     <- cumsum(density_incomp$y * diff(density_incomp$x[1:2]))

    if (is.null(xlimCDF)) {
      xlimCDF <- c(0, x$prms$tmax)
    }

    plot(density_comp$x, cdf_comp, type = "l", col = tail(cols, 2)[1],
         ylab = ylabs[4], xlab = xlabs[4],
         ylim = c(0, 1), xaxt = xaxts, yaxt = "n", xlim = xlimCDF)

    if (xaxts == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(side = 2, at = seq(0, 1, 0.5), labels = as.character(seq(0, 1, 0.5)))
    }

    lines(density_incomp$x, cdf_incomp, type = "l", col = tail(cols, 2)[2])

    abline(h = c(0, 1), col = "darkgrey", lty = 2);

    add_legend(legend, labels, tail(cols, 2), c(1, 1), c(1, 1))

  }

  # CAF
  if (showFig[5]) {
    plot_caf(x, ylimCAF, xlabs[5], ylabs[5], tail(cols, 2), xaxts, yaxts, cafBinLabels, ...)
    add_legend(legend, labels, tail(cols, 2), c(1, 1), c(1, 1))
  }

  # delta
  if (showFig[6]) {
    plot_delta(x$delta$meanBin, x$delta$meanEffect, xlimDelta, ylimDelta,
      xlabs[6], ylabs[6], xaxts, yaxts, cols[1], ...)
  }

  # delta errors
  if (showFig[7]) {
    plot_delta_errors(x$caf$Bin, x$caf$meanEffect, ylimDelta, xlabs[7], ylabs[7], xaxts, yaxts, cols[1], ...)
  }

  # rtCorrect
  if (showFig[8]) {
    plot_rt(c(1, 2), x$summary$rtCor, ylimRt, 100, xlabs[9:10], ylabs[8], yaxts, cols[1], ...)
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$rtCor, x$summary$sdRtCor)
    }
  }

  # error rate
  if (showFig[9]) {
    plot_er(c(1, 2), x$summary$perErr, ylimErr, 5, xlabs[9:10], ylabs[9], yaxts, cols[1], ...)
  }

  # rtError
  if (showFig[10]) {
    plot_rt(c(1, 2), x$summary$rtErr, ylimRt, 100, xlabs[9:10], ylabs[10], yaxts, cols[1], ...)
    if (errorBars) {
      addErrorBars(c(1, 2), x$means$rtErr, x$means$sdRtErr)
    }
  }

}


#' @title plot.dmclist: Plot delta plots from multiple dmc simulations.
#'
#' @description Plot delta function from multiple dmc simulations (i.e., dmcSims).
#'
#' @param x Output from dmcSims
#' @param ylim ylimit for delta plot
#' @param xlim xlimit for delta plot
#' @param figType delta (default), deltaErrors
#' @param xlab x-label
#' @param col color range start/end color
#' @param lineType line type ("l", "b", "o") for delta plot
#' @param legend TRUE/FALSE Show legend
#' @param legendPos legend position
#' @param legendLabels Custom legend labels
#' @param ncol number of legend columns
#' @param ... pars for legend
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' params <- list(amp = seq(20, 30, 2))
#' dmc <- dmcSims(params)
#' plot(dmc, ncol = 2, col = c("red", "green"), legendPos = "topright")
#'
#' # Example 2
#' params <- list(amp=c(10, 20), tau = seq(20, 80, 40), drc = seq(0.2, 0.6, 0.2), nTrl = 50000)
#' dmc <- dmcSims(params)
#' plot(dmc, ncol = 2, col=c("green", "blue"), ylim = c(-10, 120))
#'
#' }
#'
#' @export
plot.dmclist <- function(x,
                         ylim = NULL,
                         xlim = NULL,
                         figType = "delta",
                         xlab = "Time [ms]",
                         col=c("black", "lightgrey"),
                         lineType = "l",
                         legend = TRUE,
                         legendPos = "topleft",
                         legendLabels = NULL,
                         ncol = 1,
                         ...) {

  # colour range
  cols <- colorRampPalette(col)(length(x))

  if (tolower(figType) == "delta") {
    if (is.null(xlim)) {
      minx <- min(sapply(x, function(x) min(x$delta$meanBin)))
      maxx <- max(sapply(x, function(x) max(x$delta$meanBin)))
      xlim <- c(minx - 100, maxx + 100)
    }
    if (is.null(ylim)) {
      miny <- min(sapply(x, function(x) min(x$delta$meanEffect)))
      maxy <- max(sapply(x, function(x) max(x$delta$meanEffect)))
      ylim <- c(miny - 10, maxy + 10)
    }

    # plot
    plot(NULL, NULL, ylim = ylim, xlim = xlim, ylab = expression(paste(Delta, " RT [ms]")), xlab = xlab, ...)
    for (i in seq_along(x)) {
      lines(x[[i]]$delta$meanBin, x[[i]]$delta$meanEffect, col = cols[i], type = lineType)
    }

  } else if (tolower(figType) == "deltaerrors") {
    if (is.null(ylim)) {
      miny <- min(sapply(x, function(x) min(x$caf$meanEffect)))
      maxy <- max(sapply(x, function(x) max(x$caf$meanEffect)))
      ylim <- c(miny - 5, maxy + 5)
    }

    if (xlab == "Time [ms]") {
      xlab = "Bin"
    }

    # plot
    plot(NULL, NULL, ylim = ylim, xlim = c(min(x[[1]]$caf$Bin - 0.5), max(x[[1]]$caf$Bin + 0.5)),
      ylab = expression(paste(Delta, " ER [%]")), xlab = xlab, ...)
    for (i in seq_along(x)) {
      lines(x[[i]]$caf$Bin, x[[i]]$caf$meanEffect, col = cols[i], type = lineType)
    }

  }

  # legend
  if (is.null(legendLabels)) {
    for (i in seq_along(x)) {
      legendLabels <- c(NULL, legendLabels, paste0(names(x[[i]]$params), "=", x[[1]]$params[i, ], collapse = ", "))
    }
  }
  add_legend(legend, legendLabels, as.vector(cols), c(1), c(NA), position = legendPos, ncol = ncol)

}


#' @title plot.dmcob: Plot observed data
#'
#' @description Plot results from the output of dmcObservedData. The plot
#' can be an overall summary, or individual plots (rtCorrect, errorRate,
#' rtErrors, cdf, caf, delta, deltaErrors, all).
#'
#' @param x Output from dmcObservedData
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, deltaErrors, all
#' @param subject NULL (aggregated data across all subjects) or integer for subject number
#' @param legend TRUE/FALSE (or FUNCTION) plot legend on each plot
#' @param labels Condition labels c("Compatible", "Incompatible") default
#' @param cols Condition colours c("green", "red") default
#' @param errorBars TRUE(default)/FALSE Plot errorbars
#' @param errorBarType sd(default), or se
#' @param ylimRt ylimit for Rt plots
#' @param ylimErr ylimit for error rate plots
#' @param xlimCDF xlimit for CDF plot
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param xlimDelta xlimit for delta plot
#' @param ylimDeltaErrors ylimit for delta plot errors
#' @param xlabs TRUE/FALSE
#' @param ylabs TRUE/FALSE
#' @param xaxts TRUE/FALSE
#' @param yaxts TRUE/FALSE
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
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
#' dat <- createDF(nSubjects = 50, nTrl = 50,
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
#' dat <- createDF(nSubjects = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(420, 100, 150),
#'                            "Comp_incomp" = c(470, 100, 120)),
#'                  Error = list("Comp_comp"   = c(5, 3, 2, 1),
#'                               "Comp_incomp" = c(15, 8, 4, 2)))
#' datOb <- dmcObservedData(dat, nCAF = 4)
#' plot(datOb)
#' }
#'
#' @export
plot.dmcob <- function(x,
                       figType = "summary",
                       subject = NULL,
                       legend = TRUE,
                       labels = c("Compatible", "Incompatible"),
                       cols = c("black", "green", "red"),
                       errorBars = FALSE,
                       errorBarType = "sd",
                       ylimRt = NULL,
                       ylimErr = NULL,
                       xlimCDF = NULL,
                       ylimCAF = NULL,
                       cafBinLabels = FALSE,
                       ylimDelta = NULL,
                       xlimDelta = NULL,
                       ylimDeltaErrors = NULL,
                       xlabs = TRUE,
                       ylabs = TRUE,
                       xaxts = TRUE,
                       yaxts = TRUE,
                       resetPar = TRUE,
                       ...) {

  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  figType <- tolower(figType)
  figTypes <- c("summary", "all", "rtcorrect", "errorrate", "rterrors", "cdf", "caf", "delta", "deltaerrors")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if (length(labels) != 2) {
    stop("labels must be length 2")
  }
  if (!(is.function(legend)) && !(legend %in% c(TRUE, FALSE))) {
    stop("legend must be TRUE/FALSE or a function")
  }

  if (!is.null(subject)) {
    # select individual dataset
    if (!subject %in% x$summarySubject$Subject) {
      stop("datOb does not contain requested subject number!")
    }
    x$summary <- x$summarySubject[x$summarySubject$Subject == subject, ]
    x$delta   <- x$deltaSubject[x$deltaSubject$Subject == subject, ]
    x$caf     <- x$cafSubject[x$cafSubject$Subject == subject, ]
    errorBars <- FALSE
  }

  if (errorBars) {
    if ((!is.character(errorBarType)) | (!errorBarType %in% c("sd", "se"))) {
      stop("errorBar must be either \"sd\", or \"se\"!")
    }
  }

  showFig <- rep(FALSE, 7)

  # xlabels
  if (xlabs) {
    xlabs          <- rep("", 7)
    xlabs[c(1, 2)] <- labels
    xlabs[c(4, 6)] <- c("Time [ms]")
    xlabs[c(5, 7)] <- c("RT Bin")
  } else {
    xlabs <- rep("", 7)
  }

  # ylabels
  if (ylabs) {
    ylabs <- c("RT Correct [ms]", "Error Rate [%]", "RT Error [ms]", "CDF", "CAF",
      expression(paste(Delta, "RT [ms]")), expression(paste(Delta, "ER [%]")))
  } else {
    ylabs <- rep("", 6)
  }

  # xaxts and yaxts
  xaxts <- ifelse(xaxts, "s", "n")
  yaxts <- ifelse(yaxts, "s", "n")

  if (figType == "summary") {
    par(mar = c(4, 4, 1, 1), ...)
    layout(matrix(c(1, 4,
                    2, 5,
                    3, 6),
                  nrow = 3, ncol = 2, byrow = TRUE))
    showFig[1:6] <- TRUE
  } else if (figType == "all") {
    par(mar = c(4, 4, 1, 1), mfrow = c(1, 1), ...)
    showFig[1:7] <- TRUE
  } else {
    showFig[figTypes[3:9] %in% figType] <- TRUE
  }

  # rtCorrect
  if (showFig[1]) {
    plot_rt(c(1, 2), x$summary$rtCor, ylimRt, 100, xlabs[1:2], ylabs[1], yaxts, cols[1], ...)
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$rtCor, x$summary[[paste0(errorBarType, "RtCor")]])
    }
  }

  # errorRate
  if (showFig[2]) {
    plot_er(c(1, 2), x$summary$perErr, ylimErr, 5, xlabs[1:2], ylabs[2], yaxts, cols[1], ...)
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$perErr, x$summary[[paste0(errorBarType, "PerErr")]])
    }
  }

  # rtError
  if (showFig[3]) {
    plot_rt(c(1, 2), x$summary$rtErr, ylimRt, 100, xlabs[1:2], ylabs[3], yaxts, cols[1], ...)
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$rtErr, x$summary[[paste0(errorBarType, "RtErr")]])
    }
  }

  # CDF
  if (showFig[4]) {

    if (is.null(xlimCDF)) {
      xlimCDF <- c(min(x$delta$meanBin) - 50, max(x$delta$meanBin) + 50)
    }

    ndelta <- nrow(x$delta)
    ypoints <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    plot(x$delta$meanComp, ypoints, type = "o",
         ylim = c(0, 1), xlim = xlimCDF,
         ylab = ylabs[4], xlab = xlabs[4],
         col = tail(cols, 2)[1],
         xaxt = xaxts, yaxt = "n", ...)
    if (xaxts == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    }

    lines(x$delta$meanIncomp, ypoints, type = "o", col = tail(cols, 2)[2], ...)

    add_legend(legend, labels, tail(cols, 2), c(1, 1), c(1, 1))

  }

  # caf
  if (showFig[5]) {
    plot_caf(x, ylimCAF, xlabs[5], ylabs[5], tail(cols, 2), xaxts, yaxts, cafBinLabels, ...)
    add_legend(legend, labels, tail(cols, 2), c(1, 1), c(1, 1))
  }

  # delta
  if (showFig[6]) {
    plot_delta(x$delta$meanBin, x$delta$meanEffect, xlimDelta, ylimDelta,
      xlabs[6], ylabs[6], xaxts, yaxts, cols[1], ...)
    if (errorBars) {
      errorBarCol <- which(grepl(errorBarType, colnames(x$delta)))
      addErrorBars(x$delta$meanBin,
                   x$delta$meanEffect,
                   x$delta[[errorBarCol]],
                   arrowSize = 0.05)
    }
  }

  # delta
  if (showFig[7]) {
    plot_delta_errors(x$caf$Bin, x$caf$meanEffect, ylimDeltaErrors,
      xlabs[7], ylabs[7], xaxts, yaxts, cols[1], ...)
    if (errorBars) {
      errorBarCol <- which(grepl(errorBarType, colnames(x$caf)))
      addErrorBars(x$caf$Bin,
        x$caf$meanEffect,
        x$caf[[errorBarCol]],
        arrowSize = 0.05)
    }
  }

}

#' @title plot.dmcobs: Plot combined observed data
#'
#' @description Plot delta results from the output of dmcObservedData. The plot
#' can be an overall rtCorrect, errorRate, rtErrors, cdf, caf, delta, or all
#' of the previous plots.
#'
#' @param x Output from dmcObservedData
#' @param figType rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
#' @param subject NULL (aggregated data across all subjects) or integer for subject number
#' @param legend TRUE/FALSE (or FUNCTION) plot legend on each plot
#' @param legendLabels legend labels
#' @param labels Condition labels c("Compatible", "Incompatible") default
#' @param cols Condition colours c("green", "red") default
#' @param ltys Linetype see par
#' @param pchs Symbols see par
#' @param errorBars TRUE(default)/FALSE Plot errorbars
#' @param errorBarType sd(default), or se
#' @param ylimRt ylimit for Rt plots
#' @param ylimErr ylimit for error rate plots
#' @param xlimCDF xlimit for CDF plot
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param xlimDelta xlimit for delta plot
#' @param xlabs TRUE/FALSE
#' @param ylabs TRUE/FALSE
#' @param xaxts TRUE/FALSE
#' @param yaxts TRUE/FALSE
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' dat <- dmcCombineObservedData(flankerData, simonData)  # combine flanker/simon data
#' plot(dat, figType = "delta", xlimDelta = c(200, 700), ylimDelta = c(-20, 80),
#'      cols = c("black", "darkgrey"), pchs = c(1, 2), legend = FALSE, resetPar = FALSE)
#' legend(200, 80, legend = c("Flanker Task", "Simon Task"),
#'        col = c("black", "darkgrey"), lty = c(1, 1))
#' }
#' @export
plot.dmcobs <- function(x,
                        figType = "all",
                        subject = NULL,
                        legend = TRUE,
                        legendLabels = c(),
                        labels = c("Compatible", "Incompatible"),
                        cols = c("black", "gray"),
                        ltys = c(1, 1),
                        pchs = c(1, 1),
                        errorBars = FALSE,
                        errorBarType = "sd",
                        ylimRt = NULL,
                        ylimErr = NULL,
                        xlimCDF = NULL,
                        ylimCAF = NULL,
                        cafBinLabels = FALSE,
                        ylimDelta = NULL,
                        xlimDelta = NULL,
                        xlabs = TRUE,
                        ylabs = TRUE,
                        xaxts = TRUE,
                        yaxts = TRUE,
                        resetPar = TRUE,
                        ...) {

  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  # cycle
  if (length(x) > 2) {
    cols <- rep(cols, ceiling(length(x)/length(cols)))
    ltys <- rep(ltys, ceiling(length(x)/length(ltys)))
    pchs <- rep(ltys, ceiling(length(x)/length(pchs)))
  }

  figType <- tolower(figType)
  figTypes <- c("all", "rtcorrect", "errorrate", "rterrors", "cdf", "caf", "delta")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if (length(labels) != 2) {
    stop("labels must be length 2")
  }
  if (!(is.function(legend)) && !(legend %in% c(TRUE, FALSE))) {
    stop("legend must be TRUE/FALSE or a function")
  }

  if (!is.null(subject)) {
    # select individual subject dataset
    for (i in 1:length(x)) {
      if (!subject %in% x[[i]]$summarySubject$Subject) {
        stop("datOb does not contain requested subject number!")
      }
      x[[i]]$summary <- x[[i]]$summarySubject[x[[i]]$summarySubject$Subject == subject, ]
      x[[i]]$delta   <- x[[i]]$deltaSubject[x[[i]]$deltaSubject$Subject == subject, ]
      x[[i]]$caf     <- x[[i]]$cafSubject[x[[i]]$cafSubject$Subject == subject, ]
    }
    errorBars <- FALSE
  }

  if (length(legendLabels) == 0) {
    legendLabels <- paste0("Cond ", seq_along(x))
  }

  if (errorBars) {
    if ((!is.character(errorBarType)) | (!errorBarType %in% c("sd", "se"))) {
      stop("errorBar must be either \"sd\", or \"se\"!")
    }
  }

  showFig <- rep(FALSE, 6)

  # xlabels
  if (xlabs) {
    xlabs          <- rep("", 6)
    xlabs[c(1, 2)] <- labels
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

  # xaxts and yaxts
  xaxts <- ifelse(xaxts, "s", "n")
  yaxts <- ifelse(yaxts, "s", "n")

  if (figType == "all") {
    par(mar = c(4, 4, 1, 1), mfrow = c(1, 1), ...)
    showFig[1:6] <- TRUE
  } else {
    showFig[figTypes[2:7] %in% figType] <- TRUE
  }

  # rtCorrect
  if (showFig[1]) {

    if (is.null(ylimRt)) {
      minx <- min(sapply(x, function(x) min(x$summary$rtCor)))
      maxx <- max(sapply(x, function(x) max(x$summary$rtCor)))
      ylimRt <- c(minx - 100, maxx + 100)
    }

    plot_rt(NULL, NULL, ylimRt, 0, xlabs[1:2], ylabs[1], yaxts, cols[1], ...)
    for (i in seq_along(x)) {
      lines(c(1, 2), x[[i]]$summary$rtCor, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1, 2), x[[i]]$summary$rtCor, x[[i]]$summary[[paste0(errorBarType, "RtCor")]])
      }
    }
    add_legend(legend, legendLabels, cols, ltys, pchs)

  }

  # errorRate
  if (showFig[2]) {

    if (is.null(ylimErr)) {
      miny    <- min(sapply(x, function(x) min(x$summary$perErr)))
      maxy    <- max(sapply(x, function(x) max(x$summary$perErr)))
      ylimErr <- c(0, maxy + 10)
    }

    plot_er(NULL, NULL, ylimErr, 0, xlabs[1:2], ylabs[2], yaxts, cols[1], ...)
    for (i in seq_along(x)) {
      lines(c(1, 2), x[[i]]$summary$perErr, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1, 2), x[[i]]$summary$perErr, x[[i]]$summary[[paste0(errorBarType, "PerErr")]])
      }
    }
    add_legend(legend, legendLabels, cols, ltys, pchs)

  }

  # rtError
  if (showFig[3]) {

    if (is.null(ylimRt)) {
      miny  <- min(sapply(x, function(x) min(x$summary$rtErr)))
      maxy  <- max(sapply(x, function(x) max(x$summary$rtErr)))
      ylimRt <- c(miny - 100, maxy + 100)
    }

    plot_rt(NULL, NULL, ylimRt, 0, xlabs[1:2], ylabs[3], yaxts, cols[1], ...)
    for (i in seq_along(x)) {
      lines(c(1, 2), x[[i]]$summary$rtErr, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1, 2), x[[i]]$summary$rtErr, x[[i]]$summary[[paste0(errorBarType, "RtErr")]])
      }
    }
    add_legend(legend, legendLabels, cols, ltys, pchs)

  }

  # CDF
  if (showFig[4]) {

    if (is.null(xlimCDF)) {
      minx  <- min(sapply(x, function(x) min(x$delta$meanBin)))
      maxx  <- max(sapply(x, function(x) max(x$delta$meanBin)))
      xlimCDF <- c(minx - 100, maxx + 100)
    }

    plot(NULL, NULL,
         ylim = c(0, 1), xlim = xlimCDF,
         ylab = ylabs[4], xlab = xlabs[4],
         xaxt = xaxts, yaxt = "n", ...)
    if (xaxts == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxts ==  "s") {
      axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    }

    ndelta <- nrow(x[[1]]$delta)
    ypoints <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    for (i in seq_along(x)) {
      lines(x[[i]]$delta$meanComp, ypoints, type = "o",
           ylim = c(0, 1), xlim = c(200, 1000),
           ylab = ylabs[4], xlab = xlabs[4],
           col = tail(cols, 2)[1], lty = ltys[i], pch = pchs[i],
           xaxt = xaxts, yaxt = "n", ...)
      lines(x[[i]]$delta$meanIncomp, ypoints, type = "o", col = tail(cols, 2)[2], ...)
    }

    add_legend(legend, legendLabels, cols, ltys, pchs)

  }

  # caf
  if (showFig[5]) {

    if (is.null(ylimCAF)) {
      ylimCAF <- c(0, 1)
    }

    nCAF <- length(x[[1]]$caf$Bin)
    plot(NULL, NULL,
         ylim = ylimCAF,
         ylab = ylabs[5], xlab = xlabs[5], xlim = c(0.5, nCAF + 0.5),
         xaxt = "n", yaxt = "n", ...)
    if (xaxts == "n") {
      axis(side = 1, labels = FALSE)  # keep tick marks
    } else if (xaxts == "s" | cafBinLabels) {
      if (cafBinLabels) {
        stepCAF <- 100 / nCAF
        cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
        axis(1, at = seq(1, nCAF, 1), labels = cafLabels, ...)
      } else {
        axis(1, at = seq(1, nCAF, 1), ...)
      }
    } else {
      axis(side = 1, labels = FALSE)
    }

    if (yaxts == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)), ...)
    }

    for (i in seq_along(x)) {
      lines(x[[i]]$caf$accPerComp,   type = "o", col = tail(cols, 2)[1], lty = ltys[i], pch = pchs[i], ...)
      lines(x[[i]]$caf$accPerIncomp, type = "o", col = tail(cols, 2)[2], lty = ltys[i], pch = pchs[i], ...)
    }

    add_legend(legend, legendLabels, cols, ltys, pchs)

  }

  # delta
  if (showFig[6]) {

    if (is.null(xlimDelta)) {
      minx  <- min(sapply(x, function(x) min(x$delta$meanBin)))
      maxx  <- max(sapply(x, function(x) max(x$delta$meanBin)))
      xlimDelta <- c(minx - 100, maxx + 100)
    }

    if (is.null(ylimDelta)) {
      miny  <- min(sapply(x, function(x) min(x$delta$meanEffect)))
      maxy  <- max(sapply(x, function(x) max(x$delta$meanEffect)))
      ylimDelta <- c(miny - 100, maxy + 100)
    }

    plot(NULL, NULL,
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = ylabs[6], xlab = xlabs[6],
         xaxt = xaxts, yaxt = yaxts, ...)
    axis(side = 1, labels = FALSE)
    axis(2, labels = FALSE)

    for (i in seq_along(x)) {
      lines(x[[i]]$delta$meanBin, x[[i]]$delta$meanEffect, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i])
      if (errorBars) {
        errorBarCol <- which(grepl(errorBarType, colnames(x$delta)))
        addErrorBars(x[[i]]$delta$meanBin,
                     x[[i]]$delta$meanEffect,
                     x[[i]]$delta[[errorBarCol]],
                     arrowSize = 0.05)
      }
    }

    add_legend(legend, legendLabels, cols, ltys, pchs)

  }

}


#' @title plot.dmcfit: Plot observed + fitted data
#'
#' @description Plot the simulation results from the output of dmcFit. The plot
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
#' @param subject NULL (aggregated data across all subjects) or integer for subject number
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
#' @param legend TRUE/FALSE (or FUNCTION) plot legend on each plot
#' @param labels Condition labels c("Compatible", "Incompatible", "Observed", "Predicted") default
#' @param cols Condition colours c("green", "red") default
#' @param ylimRt ylimit for Rt plots
#' @param ylimErr ylimit for error rate plots
#' @param xlimCDF ylimit for CDF plot
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param xlimDelta xlimit for delta plot
#' @param xlabs TRUE/FALSE
#' @param ylabs TRUE/FALSE
#' @param xaxts TRUE/FALSE
#' @param yaxts TRUE/FALSE
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' resTh <- dmcFit(flankerData, nTrl = 5000)
#' plot(resTh, flankerData, figType = "rtcorrect")
#'
#' # Example 2
#' resTh <- dmcFit(flankerData, nTrl = 5000)
#' plot(resTh, flankerData)
#' plot(resTh, flankerData, figType = "all")
#'
#' # Example 3
#' resTh <- dmcFit(simonData, nTrl = 5000)
#' plot(resTh, simonData)
#' }
#'
#' @export
plot.dmcfit <- function(x,
                        y,
                        subject = NULL,
                        figType = "summary",
                        legend = TRUE,
                        labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
                        cols = c("black", "green", "red"),
                        ylimRt = NULL,
                        ylimErr = NULL,
                        xlimCDF = NULL,
                        ylimCAF = NULL,
                        cafBinLabels = FALSE,
                        ylimDelta = NULL,
                        xlimDelta = NULL,
                        xlabs = TRUE,
                        ylabs = TRUE,
                        xaxts = TRUE,
                        yaxts = TRUE,
                        resetPar = TRUE,
                        ...) {

  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  figType <- tolower(figType)
  figTypes <- c("summary", "all", "rtcorrect", "errorrate", "rterrors", "cdf", "caf", "delta")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if (!(is.function(legend)) && !(legend %in% c(TRUE, FALSE))) {
    stop("legend must be TRUE/FALSE or a function")
  }
  if (length(labels) != 4) {
    stop("labels must be length 4")
  }

  if (!is.null(subject)) {
    subjects <- which(!unlist(lapply(x, is.null)))
    if (!subject %in% subjects) {
      stop("datOb (y) does not contain requested subject number!")
    }

    x <- x[[subject]]
    if (is.null(x)) {
      stop("datTh (x) does not contain individual fits!")
    }

    y$summary <- y$summarySubject[y$summarySubject$Subject == subject, ]
    y$delta   <- y$deltaSubject[y$deltaSubject$Subject == subject, ]
    y$caf     <- y$cafSubject[y$cafSubject$Subject == subject, ]

  }

  showFig <- rep(FALSE, 6)

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

  # xaxts and yaxts
  xaxts <- ifelse(xaxts, "s", "n")
  yaxts <- ifelse(yaxts, "s", "n")

  if (figType == "summary") {
    par(mar = c(4, 4, 1, 1), ...)
    layout(matrix(c(1, 4,
                    2, 5,
                    3, 6),
                  nrow = 3, ncol = 2, byrow = TRUE))
    showFig[1:6] <- TRUE
  } else if (figType == "all") {
    par(mar = c(4, 4, 1, 1), mfrow = c(1, 1), ...)
    showFig[1:6] <- TRUE
  } else {
    showFig[figTypes[3:8] %in% figType] <- TRUE
  }

  # rtCorrect
  if (showFig[1]) {
    plot_rt(c(1, 2), y$summary$rtCor, ylimRt, 100, xlabs[1:2], ylabs[1], yaxts, cols[1], ...)
    lines(c(1, 2), c(x$summary$rtCor), type = "o", lty = 2, ...)
    add_legend(legend, labels[3:4], c(cols[1], cols[1]), c(1, 2), c(1, 1))
  }

  # errorRate
  if (showFig[2]) {
    plot_er(c(1, 2), y$summary$perErr, ylimErr, 5, xlabs[1:2], ylabs[2], yaxts, cols[1], ...)
    lines(c(1, 2), x$summary$perErr, type = "b", lty = 2, ...)
    add_legend(legend, labels[3:4], c(cols[1], cols[1]), c(1, 2), c(1, 1))
  }

  # rt Error
  if (showFig[3]) {
    plot_rt(c(1, 2), y$summary$rtErr, ylimRt, 100, xlabs[1:2], ylabs[3], yaxts, cols[1], ...)
    lines(c(1, 2), x$summary$rtErr, type = "b", lty = 2, ...)
    add_legend(legend, labels[3:4], c(cols[1], cols[1]), c(1, 2), c(1, 1))
  }

  # cdf
  if (showFig[4]) {

    if (is.null(xlimCDF)) {
      xlimCDF <- c(min(x$delta$meanComp) - 100, max(x$delta$meanComp) + 100)
    }

    ndelta <- nrow(y$delta)
    ypoints <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    plot(y$delta$meanComp, ypoints, type = "p",
         ylim = c(0, 1), xlim = xlimCDF,
         ylab = ylabs[4], xlab = xlabs[4],
         yaxt = "n", col = tail(cols, 2)[1],
         xaxt = xaxts, yaxt = "n", ...)
    if (xaxts == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    }

    lines(y$delta$meanIncomp, ypoints, type = "p", col = tail(cols, 2)[2], ...)
    lines(x$delta$meanComp,   ypoints, type = "l", col = tail(cols, 2)[1], ...)
    lines(x$delta$meanIncomp, ypoints, type = "l", col = tail(cols, 2)[2], ...)

    llabels <- c(paste(labels[1], labels[3], sep = " "),
                 paste(labels[2], labels[3], sep = " "),
                 paste(labels[1], labels[4], sep = " "),
                 paste(labels[2], labels[4], sep = " "))
    add_legend(legend, llabels, tail(cols, 2), c(0, 0, 1, 1), c(1, 1, NA, NA))

  }

  # caf
  if (showFig[5]) {

    plot_caf(x, ylimCAF, xlabs[5], ylabs[5], tail(cols, 2)[1], xaxts, yaxts, cafBinLabels, type = "l", ...)
    lines(x$caf$accPerIncomp, type = "l", col = tail(cols, 2)[2], ...)
    lines(y$caf$accPerComp,   type = "p", col = tail(cols, 2)[1], ...)
    lines(y$caf$accPerIncomp, type = "p", col = tail(cols, 2)[2], ...)

    llabels <- c(paste(labels[1], labels[3], sep = " "),
                 paste(labels[2], labels[3], sep = " "),
                 paste(labels[1], labels[4], sep = " "),
                 paste(labels[2], labels[4], sep = " "))
    add_legend(legend, llabels, tail(cols, 2), c(0, 0, 1, 1), c(1, 1, NA, NA))

  }

  # delta
  if (showFig[6]) {
     plot_delta(x$delta$meanBin, x$delta$meanEffect, xlimDelta, ylimDelta,
        xlabs[6], ylabs[6], xaxts, yaxts, cols[1], type = "l", ...)
     lines(y$delta$meanBin, y$delta$meanEffect, type = "p", ...)
    add_legend(legend, labels[3:4], cols[1], c(0, 1), c(1, NA))
  }

}


#' @title addErrorBars: Add errorbars to plot.
#'
#' @description Add error bars to current plot (uses base arrows function).
#'
#' @param xpos x-position of data-points
#' @param ypos y-position of data-points
#' @param errorSize +- size of error bars
#' @param arrowSize Width of the errorbar arrow
#'
#' @return Plot (no return value)
#'
#' @examples
#' # Example 1
#' plot(c(1, 2), c(450, 500), xlim = c(0.5, 2.5), ylim = c(400, 600), type = "o")
#' addErrorBars(c(1, 2), c(450, 500), errorSize = c(20, 20))
#'
#' # Example 2
#' plot(c(1, 2), c(450, 500), xlim = c(0.5, 2.5), ylim = c(400, 600), type = "o")
#' addErrorBars(c(1, 2), c(450, 500), errorSize = c(20, 40), arrowSize = 0.1)
#'
#' @export
addErrorBars <- function(xpos, ypos, errorSize, arrowSize = 0.1) {
  errBars <- as.data.frame(cbind(xpos, ypos, errorSize))
  with(errBars, arrows(xpos, ypos - errorSize,
                       xpos, ypos + errorSize,
                       arrowSize, 90, 3))
}


#################################### INTERNAL PLOT FUNCTIONS ###########################################################
plot_rt <- function(x, y, ylim, ylimOffset, xlabs, ylab, yaxts, col, ...) {
  if (is.null(ylim)) {
    ylim <- c(min(y) - ylimOffset, max(y) + ylimOffset)
  }
  plot(x, y, type = "o", col = col,
    ylim = ylim, xlim = c(0.5, 2.5),
    ylab = ylab, xlab = "",
    xaxt = "n",  yaxt = yaxts, ...)
  axis(1, at = c(1, 2), labels = xlabs)
  axis(2, labels = FALSE)
}

plot_er <- function(x, y, ylim, ylimOffset, xlabs, ylab, yaxts, col, ...) {
  if (is.null(ylim)) {
    ylim <- c(0, max(y) + ylimOffset)
  }
  plot(x, y, type = "o", col = col,
    ylim = ylim, xlim = c(0.5, 2.5),
    ylab = ylab, xlab = "",
    xaxt = "n",  yaxt = yaxts, ...)
  axis(1, at = c(1, 2), labels = xlabs)
  axis(2, labels = FALSE)
}

plot_caf <- function(x, ylim, xlab, ylab, cols, xaxts, yaxts, cafBinLabels, type = "o", ...) {
  if (is.null(ylim)) {
    ylim <- c(0, 1)
  }

  plot(x$caf$accPerComp, type = type,
    ylim = ylim, ylab = ylab,  xlab = xlab,
    xaxt = "n",  yaxt = "n",
    col = cols[1], ...)

  if (xaxts == "n") {
    axis(side = 1, labels = FALSE)  # keep tick marks
  } else if (xaxts == "s" | cafBinLabels) {
    nCAF <- length(x$caf$Bin)
    if (cafBinLabels) {
      stepCAF <- 100 / nCAF
      cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
      axis(1, at = seq(1, nCAF, 1), labels = cafLabels, ...)
    } else {
      axis(1, at = seq(1, nCAF, 1), ...)
    }
  } else {
    axis(side = 1, labels = F)
  }

  if (yaxts == "n") {
    axis(side = 2, labels = FALSE)  # keep tick marks
  } else if (yaxts == "s") {
    axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)), ...)
  }

  lines(x$caf$accPerIncomp, col = cols[2], type = "o", ...)

}

plot_delta <- function(x, y, xlim, ylim, xlab, ylab, xaxts, yaxts, col, type = "o", ...) {
  if (is.null(xlim)) {
    xlim <- c(min(x) - 50, max(x) + 50)
  }
  if (is.null(ylim)) {
    ylim <- c(min(y) - 50, max(y) + 50)
  }
  plot(x, y, type = type, col = col,
    ylim = ylim, xlim = xlim, ylab = ylab,  xlab = xlab,
    xaxt = xaxts, yaxt = yaxts, ...)
  axis(side = 1, labels = FALSE)
  axis(side = 2, labels = FALSE)
}

plot_delta_errors <- function(x, y, ylim, xlab, ylab, xaxts, yaxts, col, type = "o", ...) {

  if (is.null(ylim)) {
    ylim <- c(min(y) - 5, max(y) + 5)
  }

  plot(x, y, type = type, col = col,
    ylim = ylim, ylab = ylab,  xlab = xlab,
    xaxt = xaxts, yaxt = yaxts, ...)
  axis(side = 1, labels = FALSE)
  axis(side = 2, labels = FALSE)
}

add_legend <- function(legend, labels, cols, ltys, pchs, position = "bottomright", inset=c(0.05, 0.05), ...) {
  if (is.function(legend)) {
    legend()
  } else if (legend == TRUE) {
    legend(position, legend = labels, col = cols, lty = ltys, pch = pchs, inset = inset, ...)
  }
}
