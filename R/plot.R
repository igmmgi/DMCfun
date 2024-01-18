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
#' caf, delta, deltaErrors, deltaER, rtCorrect, rtErrors, errorRate, all
#' @param xlimActivation xlimit for activation plot
#' @param ylimActivation ylimit for activation plot
#' @param xlimTrials xlimit for trials plot
#' @param ylimTrials ylimit for trials plot
#' @param xlimPDF xlimit for PDF plot
#' @param ylimPDF ylimit for PDF plot
#' @param xlimCDF xlimit for CDF plot
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param xlimDelta xlimit for delta plot (Default is 0 to tmax)
#' @param ylimRt ylimit for rt plot
#' @param ylimErr ylimit for er plot
#' @param labels Condition labels c("Compatible", "Incompatible") default
#' @param cols Condition colours c("green", "red") default
#' @param errorBars TRUE/FALSE
#' @param xlabs TRUE/FALSE
#' @param ylabs TRUE/FALSE
#' @param xaxts TRUE/FALSE
#' @param yaxts TRUE/FALSE
#' @param xylabPos 2
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param legend TRUE/FALSE
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
#' dmc = dmcSim(tau = 120)
#' plot(dmc)
#'
#' # Example 4
#' dmc = dmcSim()
#' plot(dmc, figType = "all")
#'
#' }
#'
#' @export
plot.dmcsim <- function(
    x,
    figType = "summary1",
    xlimActivation = NULL,
    ylimActivation = NULL,
    xlimTrials = NULL,
    ylimTrials = NULL,
    xlimPDF = NULL,
    ylimPDF = NULL,
    xlimCDF = NULL,
    ylimCAF = NULL,
    cafBinLabels = FALSE,
    ylimDelta = NULL,
    xlimDelta = NULL,
    ylimRt = NULL,
    ylimErr = NULL,
    labels = c("Compatible", "Incompatible"),
    cols = c("green", "red"),
    errorBars = FALSE,
    xlabs = TRUE,
    ylabs = TRUE,
    xaxts = TRUE,
    yaxts = TRUE,
    xylabPos = 2,
    resetPar = TRUE,
    legend = TRUE,
    ...)
{

  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  figType <- tolower(figType)
  figTypes <- c("summary1", "summary2", "summary3", "all", "activation", "trials",
                "pdf", "cdf", "caf", "delta", "deltaerrors", "deltaer", "rtcorrect", "errorrate",  "rterrors")
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

  # x-labels
  if (xlabs) {
    xlabs[c(1:4, 6, 8)] <- c("Time [ms]")
    xlabs[c(5, 7)]      <- c("RT Bin")
    xlabs[c(10:11)]     <- c(labels)
  } else {
    xlabs <- rep("", 11)
  }

  # y-labels
  if (ylabs) {
    ylabs <- c("E[X(t)]", "X(t)", "PDF", "CDF", "CAF",
               expression(paste(Delta, " RT [ms]")), expression(paste(Delta, " RT [ms]")),
               expression(paste(Delta, " ER [%]")),
               "RT Correct [ms]", "Error Rate [%]", "RT Error [ms]")
  } else {
    ylabs <- rep("", 11)
  }

  # x-axts and y-axtsA
  xaxt <- ifelse(xaxts, "s", "n")
  yaxt <- ifelse(yaxts, "s", "n")

  showFig <- rep(FALSE, 11)
  if (figType == "summary1") {
    par(mar = c(3, 4, 1, 1), ...)
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
    par(mar = c(3, 4, 1, 1), ...)
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
    par(mar = c(2, 4, 1, 2), mfrow = c(3, 1), ...)
    showFig[9:11] <- TRUE
  } else if (figType == "all") {
    par(mar = c(4, 4, 2, 2), mfrow = c(1, 1), ...)
    showFig[1:11] <- TRUE
  } else {
    showFig[figTypes[5:15] %in% figType] <- TRUE
  }

  # activation
  if (showFig[1] & !is.null(x$trials)) {
    plot_activation(x, labels, cols, xlabs[1], ylabs[1], xlimActivation, ylimActivation,
                    xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, legend = legend, ... )
  }

  # individual trials
  if (showFig[2] & !is.null(x$trials)) {
    plot_trials(x, labels, cols, xlabs[2], ylabs[2], xlimActivation, ylimActivation,
                xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, legend = legend, ...)
  }

  # PDF
  if (showFig[3]) {
    plot_pdf(resTh = x, labels = labels, cols = cols, xlab = xlabs[3], ylab = ylabs[3],
             xlim = xlimPDF, ylim = ylimPDF,  xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos,
             legend = legend, legend.parameters = list(cex = ifelse(figType == "summary1", 0.75, 1)), ...)
  }

  # CDF
  if (showFig[4]) {
    plot_cdf(resTh = x, labels = labels, cols = cols, xlab = xlabs[4], ylab = ylabs[4],
             xlim = xlimCDF,  xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos,
             legend = legend, legend.parameters = list(cex = ifelse(figType == "summary1", 0.75, 1)), ...)
  }

  # CAF
  if (showFig[5]) {
    plot_caf(resTh = x, labels = labels, cols = cols, xlab = xlabs[5], ylab = ylabs[5],
             ylim = ylimCAF, xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, cafBinLabels = cafBinLabels,
             legend = legend, ...)
  }

  # delta/delta errors
  if (showFig[6]) {
    plot_delta(resTh = x, figType = "delta", labels = NULL, xlim = xlimDelta, ylim = ylimDelta,
               xlab = xlabs[6], ylab = ylabs[6], xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, ...)
  }

  # delta/delta errors
  if (showFig[7] & !is.null(x$deltaErrors)) {
    plot_delta(resTh = x, figType = "deltaErrors", labels = NULL, xlim = xlimDelta, ylim = ylimDelta,
               xlab = xlabs[7], ylab = ylabs[7], xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, ...)
  }

  # deltaER
  if (showFig[8]) {
    plot_delta_er(resTh = x, labels = NULL, xlim = xlimDelta, ylim = ylimDelta,
               xlab = xlabs[8], ylab = ylabs[8], xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, ...)
  }

  # rtCorrect
  if (showFig[9]) {
    plot_beh(resTh = x, figType = "rtcorrect", xlabs = labels, ylab = ylabs[9],
             ylim = ylimRt, xaxt = "n", yaxt = yaxt, xylabPos = xylabPos, ...)
    if (errorBars) addErrorBars(c(1, 2), x$summary$rtCor, x$summary$sdRtCor)
  }

  # error rate
  if (showFig[10]) {
    plot_beh(resTh = x, figType = "errorrate", xlabs = labels, ylab = ylabs[10],
             ylim = ylimErr, xaxt = "n", yaxt = yaxt, xylabPos = xylabPos, ...)
  }

  # rtError
  if (showFig[11]) {
    plot_beh(resTh = x, figType = "rterrors", xlabs = labels, ylab = ylabs[11],
             ylim = ylimRt, xaxt = "n", yaxt = yaxt, xylabPos = xylabPos, ...)
    if (errorBars) addErrorBars(c(1, 2), x$summary$rtErr, x$summary$sdRtErr)
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
#' @param ylab y-label
#' @param xylabPos x/y label position
#' @param col color range start/end color
#' @param lineType line type ("l", "b", "o") for delta plot
#' @param legend TRUE/FALSE Show legend
#' @param legend.parameters list
#' @param ... pars for plot
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' params <- list(amp = seq(20, 30, 2))
#' dmc <- dmcSims(params)
#' plot(dmc, col = c("red", "green"), legend.parameters = list(x = "topright", ncol=2))
#'
#' # Example 2
#' params <- list(amp=c(10, 20), tau = c(20, 40), drc = c(0.2, 0.6), nTrl = 50000)
#' dmc <- dmcSims(params)
#' plot(dmc, col=c("green", "blue"), ylim = c(-10, 120), legend.parameters=list(ncol=2))
#'
#' }
#'
#' @export
plot.dmclist <- function(x,
                         ylim = NULL,
                         xlim = NULL,
                         figType = "delta",
                         xlab = "Time [ms]",
                         ylab = expression(paste(Delta, "Time [ms]")),
                         xylabPos = 2,
                         col=c("black", "lightgrey"),
                         lineType = "l",
                         legend = TRUE,
                         legend.parameters = list(),
                         ...) {

  if (!tolower(figType) %in% c("delta", "deltaerrors")) {
    stop("plotting multiple dmcSims only possible with figType = delta/deltaErrors")
  }

  # find suitable x/y limits
  idx <- ifelse(tolower(figType) == "delta", 4, 5)
  if (is.null(xlim)) {
    minx <- min(sapply(x, function(x) min(x[[idx]]$meanBin)))
    maxx <- max(sapply(x, function(x) max(x[[idx]]$meanBin)))
    xlim <- c(minx - 100, maxx + 100)
  }
  if (is.null(ylim)) {
    miny <- min(sapply(x, function(x) min(x[[idx]]$meanEffect)))
    maxy <- max(sapply(x, function(x) max(x[[idx]]$meanEffect)))
    ylim <- c(miny - 10, maxy + 10)
  }

  cols <- colorRampPalette(col)(length(x))

  plot(NULL, NULL, ylim = ylim, xlim = xlim, ylab = "", xlab = "", ...)
  for (i in seq_along(x)) {
    lines(x[[i]][[idx]]$meanBin, x[[i]][[idx]]$meanEffect, col = cols[i], type = lineType, ...)
  }
  title(xlab = xlab, ylab = ylab, line = xylabPos, ...)

  # legend
  if (legend) {
    if (is.null(legend.parameters$legend)) {
      legendLabels <- NULL
      for (i in seq_along(x)) {
        legendLabels <- c(NULL, legendLabels, paste0(names(x[[i]]$params), "=", x[[i]]$params, collapse = ", "))
      }
    }
    delfault_legend.parameters <- list(legend = legendLabels, x = "topleft", col = cols,
                                       lty = c(1), pch = c(NA), ncol = 1, bty = "o", inset = 0)
    legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
    add_legend(legend.parameters)
  }

}

#' @title plot.dmcob: Plot observed data
#'
#' @description Plot results from the output of dmcObservedData. The plot
#' can be an overall summary, or individual plots (rtCorrect, errorRate,
#' rtErrors, cdf, caf, delta, deltaErrors, all).
#'
#' @param x Output from dmcObservedData
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, deltaErrors, deltaER, all
#' @param subject NULL (aggregated data across all subjects) or integer for subject number
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
#' @param xlabs TRUE/FALSE
#' @param ylabs TRUE/FALSE
#' @param xaxts TRUE/FALSE
#' @param yaxts TRUE/FALSE
#' @param xylabPos 2
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param legend TRUE/FALSE (or FUNCTION) plot legend on each plot
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
                       labels = c("Compatible", "Incompatible"),
                       cols = c("green", "red"),
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
                       xylabPos = 2,
                       resetPar = TRUE,
                       legend = TRUE,
                       ...) {

  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  figType <- tolower(figType)
  figTypes <- c("summary", "all", "rtcorrect", "errorrate", "rterrors", "cdf",
                "caf", "delta", "deltaerrors", "deltaer")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if (length(labels) != 2) {
    stop("labels must be length 2")
  }

  if (!is.null(subject)) {
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

  # x-labels
  if (xlabs) {
    xlabs            <- rep("", 8)
    xlabs[c(1, 2)]   <- labels
    xlabs[c(4, 6:8)] <- c("Time [ms]")
    xlabs[c(5)]      <- c("RT Bin")
  } else {
    xlabs <- rep("", 8)
  }

  # y-labels
  if (ylabs) {
    ylabs <- c("RT Correct [ms]", "Error Rate [%]", "RT Error [ms]", "CDF", "CAF",
               expression(paste(Delta, "RT [ms]")), expression(paste(Delta, "RT [ms]")),
               expression(paste(Delta, "ER [%]")))
  } else {
    ylabs <- rep("", 8)
  }

  # xaxts and yaxts
  xaxt <- ifelse(xaxts, "s", "n")
  yaxt <- ifelse(yaxts, "s", "n")

  showFig <- rep(FALSE, 8)
  if (figType == "summary") {
    par(mar = c(4, 4, 1, 1), ...)
    layout(matrix(c(1, 4,
                    2, 5,
                    3, 6),
                  nrow = 3, ncol = 2, byrow = TRUE))
    showFig[1:6] <- TRUE
  } else if (figType == "all") {
    par(mar = c(4, 4, 1, 1), mfrow = c(1, 1), ...)
    showFig[1:8] <- TRUE
  } else {
    showFig[figTypes[3:10] %in% figType] <- TRUE
  }

  # rtCorrect
  if (showFig[1]) {
    plot_beh(resOb = x, figType = "rtcorrect", xlabs = labels, ylab = ylabs[1], ylim = ylimRt,
             xaxt = "n", yaxt = yaxt, xylabPos = xylabPos, ...)
    if (errorBars) addErrorBars(c(1, 2), x$summary$rtCor, x$summary[[paste0(errorBarType, "RtCor")]])
  }

  # errorRate
  if (showFig[2]) {
    plot_beh(resOb = x, figType = "errorrate", xlabs = labels, ylab = ylabs[2], ylim = ylimErr,
             xaxt = "n", yaxt = yaxt, xylabPos = xylabPos, ...)
    if (errorBars) addErrorBars(c(1, 2), x$summary$perErr, x$summary[[paste0(errorBarType, "PerErr")]])
  }

  # rtError
  if (showFig[3]) {
    plot_beh(resOb = x, figType = "rterrors", xlabs = labels, ylab = ylabs[3], ylim = ylimErr,
             xaxt = "n", yaxt = yaxt, xylabPos = xylabPos, ...)
    if (errorBars) addErrorBars(c(1, 2), x$summary$rtErr, x$summary[[paste0(errorBarType, "RtErr")]])
  }

  # CDF
  if (showFig[4]) {
    plot_cdf(resOb = x, labels = labels, cols = cols, xlab = xlabs[4], ylab = ylabs[4], xlim = xlimCDF,
             xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos,
             legend = legend, legend.parameters = list(cex = ifelse(figType == "summary1", 0.75, 1)), ...)
  }

  # CAF
  if (showFig[5]) {
    plot_caf(resOb = x, labels = labels, cols = cols, xlab = xlabs[5], ylab = ylabs[5], ylim = ylimCAF,
             xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, cafBinLabels = cafBinLabels,
             legend = legend, ...)
  }

  # delta
  if (showFig[6]) {
    plot_delta(resOb = x, figType = "delta", xlim = xlimDelta, ylim = ylimDelta,
               xlab = xlabs[6], ylab = ylabs[6],
               xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, ...)
    if (errorBars) {
      ebc   <- which(grepl(errorBarType, colnames(x$delta)))
      xdata <- x$delta$meanBin
      ydata <- x$delta$meanEffect
      addErrorBars(xdata, ydata, x$delta[[ebc]], arrowSize = 0.05)
    }
  }

  # delta/delta Errors
  if (showFig[7]) {
    plot_delta(resOb = x, figType = "deltaErrors", xlim = xlimDelta, ylim = ylimDelta,
               xlab = xlabs[7], ylab = ylabs[7],
               xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, ...)
    if (errorBars) {
      ebc   <- which(grepl(errorBarType, colnames(x$deltaErrors)))
      xdata <- x$deltaErrors$meanBin
      ydata <- x$deltaErrors$meanEffect
      addErrorBars(xdata, ydata, x$delta[[ebc]], arrowSize = 0.05)
    }
  }

  # deltaER
  if (showFig[8]) {
    plot_delta_er(resOb = x, xlim = xlimDelta, ylim = ylimDelta, xlab = xlabs[8], ylab = ylabs[8],
                  xaxt = xaxt, yaxt = yaxt, xylabPos = xylabPos, ...)
    if (errorBars) {
      ebc   <- which(grepl(errorBarType, colnames(x$caf)))
      xdata <- x$caf$meanBin
      ydata <- x$caf$meanEffect
      addErrorBars(xdata, ydata, x$delta[[ebc]], arrowSize = 0.05)
    }
  }

}

#' @title plot.dmcobs: Plot combined observed data
#'
#' @description Plot delta results from the output of dmcObservedData. The plot
#' can be an overall rtCorrect, errorRate, rtErrors, cdf, caf, delta, deltaErrors, deltaER, or all
#' of the previous plots.
#'
#' @param x Output from dmcObservedData
#' @param figType rtCorrect, errorRate, rtErrors, cdf, caf, delta, deltaErrors, deltaER, all
#' @param subject NULL (aggregated data across all subjects) or integer for subject number
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
#' @param xylabPos 2
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param legend TRUE/FALSE
#' @param legend.parameters list
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' dat <- dmcCombineObservedData(flankerData, simonData)  # combine flanker/simon data
#' plot(dat, figType = "all", xlimDelta = c(200, 700), ylimDelta = c(-20, 80),
#'      cols = c("black", "darkgrey"), pchs = c(1, 2))
#' plot(dat, figType = "delta", xlimDelta = c(200, 700), ylimDelta = c(-20, 80),
#'      cols = c("black", "darkgrey"), pchs = c(1, 2), legend = TRUE,
#'      legend.parameters=list(x="topright", legend=c("Flanker", "Simon")))
#' }
#' @export
plot.dmcobs <- function(x,
                        figType = "all",
                        subject = NULL,
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
                        xylabPos = 2,
                        resetPar = TRUE,
                        legend = TRUE,
                        legend.parameters = list(),
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
    pchs <- rep(pchs, ceiling(length(x)/length(pchs)))
  }

  figType <- tolower(figType)
  figTypes <- c("all", "rtcorrect", "errorrate", "rterrors", "cdf", "caf", "delta", "deltaerrors", "deltaer")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if (length(labels) != 2) {
    stop("labels must be length 2")
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


  if (errorBars) {
    if ((!is.character(errorBarType)) | (!errorBarType %in% c("sd", "se"))) {
      stop("errorBar must be either \"sd\", or \"se\"!")
    }
  }

  showFig <- rep(FALSE, 8)

  # xlabels
  if (xlabs) {
    xlabs          <- rep("", 8)
    xlabs[c(1, 2)] <- labels
    xlabs[c(4, 6)] <- c("Time [ms]")
    xlabs[c(5)]    <- c("RT Bin")
  } else {
    xlabs <- rep("", 8)
  }

  # ylabels
  if (ylabs) {
    ylabs <- c("RT Correct [ms]", "Error Rate [%]", "RT Error [ms]", "CDF", "CAF", expression(Delta))
  } else {
    ylabs <- rep("", 8)
  }

  # xaxts and yaxts
  xaxt <- ifelse(xaxts, "s", "n")
  yaxt <- ifelse(yaxts, "s", "n")

  if (figType == "all") {
    par(mar = c(4, 4, 1, 1), mfrow = c(1, 1), ...)
    showFig[1:8] <- TRUE
  } else {
    showFig[figTypes[2:9] %in% figType] <- TRUE
  }

  # rtCorrect
  if (showFig[1]) {

    if (is.null(ylimRt)) {
      minx <- min(sapply(x, function(x) min(x$summary$rtCor)))
      maxx <- max(sapply(x, function(x) max(x$summary$rtCor)))
      ylimRt <- c(minx - 100, maxx + 100)
    }

    plot_beh(resTh = NULL, resOb = NULL, figType = "rtcorrect", xlabs = labels,
             ylab = ylabs[1], ylim = ylimRt, xylabPos = xylabPos, ...)
    for (i in seq_along(x)) {
      lines(c(1, 2), x[[i]]$summary$rtCor, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1, 2), x[[i]]$summary$rtCor, x[[i]]$summary[[paste0(errorBarType, "RtCor")]])
      }
    }

    if (legend) {
      delfault_legend.parameters <- list(legend = paste0("Cond ", seq_along(x)), x = "bottomright",
                                         col = cols, lty = ltys, pch = pchs, inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }

  }

  # errorRate
  if (showFig[2]) {

    if (is.null(ylimErr)) {
      miny    <- min(sapply(x, function(x) min(x$summary$perErr)))
      maxy    <- max(sapply(x, function(x) max(x$summary$perErr)))
      ylimErr <- c(0, maxy + 10)
    }

    plot_beh(resTh = NULL, resOb = NULL, figType = "errorrate", xlabs = labels,
             ylab = ylabs[2], ylim = ylimErr, xylabPos = xylabPos, ...)
    for (i in seq_along(x)) {
      lines(c(1, 2), x[[i]]$summary$perErr, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1, 2), x[[i]]$summary$perErr, x[[i]]$summary[[paste0(errorBarType, "PerErr")]])
      }
    }

    if (legend) {
      delfault_legend.parameters <- list(legend = paste0("Cond ", seq_along(x)), x = "bottomright",
                                         col = cols, lty = ltys, pch = pchs, inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }

  }

  # rtError
  if (showFig[3]) {

    if (is.null(ylimRt)) {
      miny  <- min(sapply(x, function(x) min(x$summary$rtErr)))
      maxy  <- max(sapply(x, function(x) max(x$summary$rtErr)))
      ylimRt <- c(miny - 100, maxy + 100)
    }

    plot_beh(resTh = NULL, resOb = NULL, figType = "rterrors", xlabs = labels,
             ylab = ylabs[3], ylim = ylimRt, xylabPos = xylabPos, ...)
    for (i in seq_along(x)) {
      lines(c(1, 2), x[[i]]$summary$rtErr, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1, 2), x[[i]]$summary$rtErr, x[[i]]$summary[[paste0(errorBarType, "RtErr")]])
      }
    }

    if (legend) {
      delfault_legend.parameters <- list(legend = paste0("Cond ", seq_along(x)), x = "bottomright",
                                         col = cols, lty = ltys, pch = pchs, inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }

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
         ylab = "", xlab = "",
         xaxt = xaxt, yaxt = "n", ...)
    title(xlab = xlabs[4], line = xylabPos)
    title(ylab = ylabs[4], line = xylabPos)
    if (xaxt == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxt == "n") {
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
            xaxt = xaxt, yaxt = "n", ...)
      lines(x[[i]]$delta$meanIncomp, ypoints, type = "o", col = tail(cols, 2)[2], ...)
    }

    if (legend) {
      delfault_legend.parameters <- list(legend = paste0("Cond ", seq_along(x)), x = "bottomright",
                                         col = cols, lty = ltys, pch = pchs, inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }

  }

  # CAF
  if (showFig[5]) {

    if (is.null(ylimCAF)) {
      ylimCAF <- c(0, 1)
    }

    nCAF <- length(x[[1]]$caf$Bin)
    plot(NULL, NULL,
         ylim = ylimCAF,
         ylab = "", xlab = "", xlim = c(0.5, nCAF + 0.5),
         xaxt = "n", yaxt = "n", ...)
    title(xlab = xlabs[5], line = xylabPos)
    title(ylab = ylabs[5], line = xylabPos)
    if (xaxts == "n") {
      axis(side = 1, labels = FALSE)  # keep tick marks
    } else if (xaxt == "s" | cafBinLabels) {
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

    if (yaxt == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxt == "s") {
      axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)), ...)
    }

    for (i in seq_along(x)) {
      lines(x[[i]]$caf$accPerComp,   type = "o", col = tail(cols, 2)[1], lty = ltys[i], pch = pchs[i], ...)
      lines(x[[i]]$caf$accPerIncomp, type = "o", col = tail(cols, 2)[2], lty = ltys[i], pch = pchs[i], ...)
    }

    if (legend) {
      delfault_legend.parameters <- list(legend = paste0("Cond ", seq_along(x)),
                                         x = "bottomright", col = cols, lty = ltys, pch = pchs, inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }

  }

  # delta
  if (showFig[6]) {

    # default axis limits
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
         ylab = "", xlab = "",
         xaxt = xaxt, yaxt = yaxt, ...)
    title(xlab = xlabs[6], line = xylabPos)
    title(ylab = ylabs[6], line = xylabPos)
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

    if (legend) {
      delfault_legend.parameters <- list(legend = paste0("Cond ", seq_along(x)),
                                         x = "bottomright", col = cols, lty = ltys, pch = pchs, inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }

  }

  # deltaErrors
  if (showFig[7]) {

    # default axis limits
    if (is.null(xlimDelta)) {
      minx  <- min(sapply(x, function(x) min(x$deltaErrors$meanBin)))
      maxx  <- max(sapply(x, function(x) max(x$deltaErrors$meanBin)))
      xlimDelta <- c(minx - 100, maxx + 100)
    }
    if (is.null(ylimDelta)) {
      miny  <- min(sapply(x, function(x) min(x$deltaErrors$meanEffect)))
      maxy  <- max(sapply(x, function(x) max(x$deltaErrors$meanEffect)))
      ylimDelta <- c(miny - 100, maxy + 100)
    }

    plot(NULL, NULL,
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = "", xlab = "",
         xaxt = xaxt, yaxt = yaxt, ...)
    title(xlab = xlabs[7], line = xylabPos)
    title(ylab = ylabs[7], line = xylabPos)
    axis(side = 1, labels = FALSE)
    axis(2, labels = FALSE)

    for (i in seq_along(x)) {
      lines(x[[i]]$deltaErrors$meanBin, x[[i]]$deltaErrors$meanEffect, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i])
      if (errorBars) {
        errorBarCol <- which(grepl(errorBarType, colnames(x$deltaErrors)))
        addErrorBars(x[[i]]$deltaErrors$meanBin,
                     x[[i]]$deltaErrors$meanEffect,
                     x[[i]]$deltaErrors[[errorBarCol]],
                     arrowSize = 0.05)
      }
    }

    if (legend) {
      delfault_legend.parameters <- list(legend = paste0("Cond ", seq_along(x)),
                                         x = "bottomright", col = cols, lty = ltys, pch = pchs, inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }

  }

  # deltaER
  if (showFig[8]) {

    # default axis limits
    if (is.null(xlimDelta)) {
      minx  <- min(sapply(x, function(x) min(x$caf$meanBin)))
      maxx  <- max(sapply(x, function(x) max(x$caf$meanBin)))
      xlimDelta <- c(minx - 100, maxx + 100)
    }
    if (is.null(ylimDelta)) {
      miny  <- min(sapply(x, function(x) min(x$caf$meanEffect)))
      maxy  <- max(sapply(x, function(x) max(x$caf$meanEffect)))
      ylimDelta <- c(miny - 2, maxy + 2)
    }

    plot(NULL, NULL,
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = "", xlab = "",
         xaxt = xaxt, yaxt = yaxt, ...)
    title(xlab = xlabs[8], line = xylabPos)
    title(ylab = ylabs[8], line = xylabPos)
    axis(side = 1, labels = FALSE)
    axis(2, labels = FALSE)

    for (i in seq_along(x)) {
      lines(x[[i]]$caf$meanBin, x[[i]]$caf$meanEffect, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i])
      if (errorBars) {
        errorBarCol <- which(grepl(errorBarType, colnames(x$caf)))
        addErrorBars(x[[i]]$caf$meanBin,
                     x[[i]]$caf$meanEffect,
                     x[[i]]$caf[[errorBarCol]],
                     arrowSize = 0.05)
      }
    }

    if (legend) {
      delfault_legend.parameters <- list(legend = paste0("Cond ", seq_along(x)), x = "bottomright",
                                         col = cols, lty = ltys, pch = pchs, inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }

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
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
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
#' @param xylabPos 2
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param legend TRUE/FALSE
#' @param legend.parameters list
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' resTh <- dmcFit(flankerData, nTrl = 5000)
#' plot(resTh, flankerData)
#' plot(resTh, flankerData, figType = "deltaErrors")
#'
#' # Example 2
#' resTh <- dmcFit(simonData, nTrl = 5000)
#' plot(resTh, simonData)
#' }
#'
#' @export
plot.dmcfit <- function(x,
                        y,
                        figType = "summary",
                        labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
                        cols = c("green", "red"),
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
                        xylabPos = 2,
                        resetPar = TRUE,
                        legend = TRUE,
                        legend.parameters = list(legend = c("Observed", "Predicted")),
                        ...) {

  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  figType <- tolower(figType)
  figTypes <- c("summary", "all", "rtcorrect", "errorrate", "rterrors", "cdf", "caf", "delta", "deltaerrors", "deltaer")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if (length(labels) != 4) {
    stop("labels must be length 4")
  }

  # x-labels
  if (xlabs) {
    xlabs             <- rep("", 8)
    xlabs[c(1, 2)]    <- c(labels[1], labels[2])
    xlabs[c(4, 6, 7)] <- c("Time [ms]")
    xlabs[c(5)]    <- c("RT Bin")
  } else {
    xlabs <- rep("", 8)
  }

  # y-labels
  if (ylabs) {
    ylabs <- c("RT Correct [ms]", "Error Rate [%]", "RT Error [ms]", "CDF", "CAF",
               expression(paste(Delta, " RT [ms]")), expression(paste(Delta, " RT [ms]")),
               expression(paste(Delta, " ER [%]")))
  } else {
    ylabs <- rep("", 8)
  }

  # xaxts and yaxts
  xaxt <- ifelse(xaxts, "s", "n")
  yaxt <- ifelse(yaxts, "s", "n")

  showFig <- rep(FALSE, 8)
  if (figType == "summary") {
    par(mar = c(4, 4, 1, 1), ...)
    layout(matrix(c(1, 4,
                    2, 5,
                    3, 6),
                  nrow = 3, ncol = 2, byrow = TRUE))
    showFig[1:6] <- TRUE
  } else if (figType == "all") {
    par(mar = c(4, 4, 1, 1), mfrow = c(1, 1), ...)
    showFig[1:8] <- TRUE
  } else {
    showFig[figTypes[3:10] %in% figType] <- TRUE
  }

  # rtCorrect
  if (showFig[1]) {
    plot_beh(resTh = x, resOb = y, figType = "rtcorrect", xlabs = labels[1:2],
             ylab = ylabs[1], ylim = ylimRt,  xylabPos = xylabPos,
             legend = legend, legend.parameters = list(legend = labels[3:4]))
  }

  # errorRate
  if (showFig[2]) {
    plot_beh(resTh = x, resOb = y, figType = "errorrate", xlabs = labels[1:2],
             ylab = ylabs[2], ylim = ylimRt, xylabPos = xylabPos,
             legend = legend, legend.parameters = list(legend = labels[3:4]))
  }

  # rt Error
  if (showFig[3]) {
    plot_beh(resTh = x, resOb = y, figType = "rterrors", xlabs = labels[1:2],
             ylab = ylabs[3], ylim = ylimRt, xylabPos = xylabPos,
             legend = legend, legend.parameters = list(legend = labels[3:4]))
  }

  # CDF
  if (showFig[4]) {
    plot_cdf(resTh = x, resOb = y, labels = labels, cols = cols, xlab = xlabs[4], ylab = ylabs[4], xlim = xlimCDF, xylabPos = xylabPos,
             legend = legend, legend.parameters = list(cex = ifelse(figType == "summary", 0.75, 1)), ...)
  }

  # CAF
  if (showFig[5]) {
    plot_caf(resTh = x, resOb = y, labels = labels, cols = cols, xlab = xlabs[5], ylab = ylabs[5], ylim = ylimCAF, xylabPos = xylabPos,
             legend = legend, legend.parameters = list(cex = ifelse(figType == "summary", 0.75, 1)), ...)
  }

  # delta
  if (showFig[6]) {
    plot_delta(resTh = x, resOb = y, figType = "delta", xlim = xlimDelta, ylim = ylimDelta,
               xlab = xlabs[6], ylab = ylabs[6], xaxt = xaxt, yaxt = yaxt, labels = labels[3:4],
               xylabPos = xylabPos,
               legend = legend, legend.parameters = list(cex = ifelse(figType == "summary", 0.75, 1)), ...)
  }

  # deltaErrors
  if (showFig[7] & !is.null(x$deltaErrors)) {
    plot_delta(resTh = x, resOb = y, figType = "deltaErrors", xlim = xlimDelta, ylim = ylimDelta,
               xlab = xlabs[7], ylab = ylabs[7], xaxt = xaxt, yaxt = yaxt, labels = labels[3:4],
               xylabPos = xylabPos,
               legend = legend, legend.parameters = list(cex = ifelse(figType == "summary", 0.75, 1)), ...)
  }

  # deltaER
  if (showFig[8]) {
    plot_delta_er(resTh = x, resOb = y, xlim = xlimDelta, ylim = ylimDelta,
                  xlab = xlabs[8], ylab = ylabs[8], xaxt = xaxt, yaxt = yaxt, labels = labels[3:4],
                  xylabPos = xylabPos,
                  legend = legend, legend.parameters = list(cex = ifelse(figType == "summary", 0.75, 1)), ...)
  }

}

#' @title plot.dmcfit_subject: Plot observed + fitted data
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
#' @param xylabPos 2
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param legend TRUE/FALSE
#' @param legend.parameters list
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' resTh <- dmcFit(flankerData, nTrl = 5000)
#' plot(resTh, flankerData)
#' plot(resTh, flankerData, figType = "deltaErrors")
#'
#' # Example 2
#' resTh <- dmcFit(simonData, nTrl = 5000)
#' plot(resTh, simonData)
#' }
#'
#' @export
plot.dmcfit_subject <- function(x,
                                y,
                                subject = NULL,
                                figType = "summary",
                                labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
                                cols = c("green", "red"),
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
                                xylabPos = 2,
                                resetPar = TRUE,
                                legend = TRUE,
                                legend.parameters = list(legend = c("Observed", "Predicted")),
                                ...) {

  if (is.null(subject)) {
    x <- mean(x)

  } else {

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

  plot(x, y,
       figType = figType,
       labels = labels,
       cols = cols,
       ylimRt = ylimRt,
       ylimErr = ylimErr,
       xlimCDF = xlimCDF,
       ylimCAF = ylimCAF,
       cafBinLabels = cafBinLabels,
       ylimDelta = ylimDelta,
       xlimDelta = xlimDelta,
       xlabs = xlabs,
       ylabs = ylabs,
       xaxts = xaxts,
       yaxts = yaxts,
       xylabPos = xylabPos,
       resetPar = resetPar,
       legend = legend,
       legend.parameters = legend.parameters,
       ...)

}



#' @title plot.dmcfits: Plot observed + fitted data
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
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
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
#' @param xylabPos 2
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param legend TRUE/FALSE
#' @param legend.parameters list
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' resTh <- dmcFit(flankerData, nTrl = 5000)
#' plot(resTh, flankerData)
#' plot(resTh, flankerData, figType = "deltaErrors")
#'
#' # Example 2
#' resTh <- dmcFit(simonData, nTrl = 5000)
#' plot(resTh, simonData)
#' }
#'
#' @export
plot.dmcfits <- function(x,
                         y,
                         figType = "summary",
                         labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
                         cols = c("green", "red"),
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
                         xylabPos = 2,
                         resetPar = TRUE,
                         legend = TRUE,
                         legend.parameters = list(legend = c("Observed", "Predicted")),
                         ...) {

  if (length(x) != length(y)) {
    stop("length(x) != length(y)!")
  }

  for (plt in 1:length(x)) {
    plot(x[[plt]],
         y[[plt]],
         figType           = figType,
         labels            = labels,
         cols              = cols,
         ylimRt            = ylimRt,
         ylimErr           = ylimErr,
         xlimCDF           = xlimCDF,
         ylimCAF           = ylimCAF,
         cafBinLabels      = cafBinLabels,
         ylimDelta         = ylimDelta,
         xlimDelta         = xlimDelta,
         xlabs             = xlabs,
         ylabs             = ylabs,
         xaxts             = xaxts,
         yaxts             = yaxts,
         xylabPos          = xylabPos,
         resetPar          = resetPar,
         legend            = legend,
         legend.parameters = legend.parameters,
         ...)
  }

}

#' @title plot.dmcfits_subject: Plot observed + fitted data
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
#' @param xylabPos 2
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param legend TRUE/FALSE
#' @param legend.parameters list
#' @param ... additional plot pars
#'
#' @return Plot (no return value)
#'
#' @examples
#' \donttest{
#' # Example 1
#' resTh <- dmcFit(flankerData, nTrl = 5000)
#' plot(resTh, flankerData)
#' plot(resTh, flankerData, figType = "deltaErrors")
#'
#' # Example 2
#' resTh <- dmcFit(simonData, nTrl = 5000)
#' plot(resTh, simonData)
#' }
#'
#' @export
plot.dmcfits_subject <- function(x,
                         y,
                         subject = NULL,
                         figType = "summary",
                         labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
                         cols = c("green", "red"),
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
                         xylabPos = 2,
                         resetPar = TRUE,
                         legend = TRUE,
                         legend.parameters = list(legend = c("Observed", "Predicted")),
                         ...) {

  if (!is.null(subject)) {
    # each data set for specific subject
    for (dataset in 1:length(y)) {
      plot(x[[subject]][[dataset]], y[[dataset]],
           figType = figType,
           labels = labels,
           cols = cols,
           ylimRt = ylimRt,
           ylimErr = ylimErr,
           xlimCDF = xlimCDF,
           ylimCAF = ylimCAF,
           cafBinLabels = cafBinLabels,
           ylimDelta = ylimDelta,
           xlimDelta = xlimDelta,
           xlabs = xlabs,
           ylabs = ylabs,
           xaxts = xaxts,
           yaxts = yaxts,
           xylabPos = xylabPos,
           resetPar = resetPar,
           legend = legend,
           legend.parameters = legend.parameters,
           ...
      )
    }
  } else {
    subjects = list()
    # each data set for mean across all subjects
    for (dataset in 1:length(y)) {
      for (subject in 1:length(x)) {
        subjects[[subject]] <- x[[subject]][[dataset]]
      }
      class(subjects) <- "dmcfit_subject"
      plot(mean(subjects), y[[dataset]],
           figType = figType,
           labels = labels,
           cols = cols,
           ylimRt = ylimRt,
           ylimErr = ylimErr,
           xlimCDF = xlimCDF,
           ylimCAF = ylimCAF,
           cafBinLabels = cafBinLabels,
           ylimDelta = ylimDelta,
           xlimDelta = xlimDelta,
           xlabs = xlabs,
           ylabs = ylabs,
           xaxts = xaxts,
           yaxts = yaxts,
           xylabPos = xylabPos,
           resetPar = resetPar,
           legend = legend,
           legend.parameters = legend.parameters,
           ...)
    }
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
# TO DO: Export/document any of these functions?
plot_activation <- function(
    resTh,
    labels = c("Compatible", "Incompatible"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    ylab = "E[X(t)]",
    xlim = NULL,
    ylim = NULL,
    xaxt = "s",
    yaxt = "s",
    xylabPos = 2,
    legend = TRUE,
    legend.parameters = list(),
    ...
) {

  if (is.null(resTh$trials)) {
    stop("plotting activation data requires dmcSim with fullData = TRUE")
  }

  if (is.null(xlim)) xlim <- c(0, resTh$prms$tmax)
  if (is.null(ylim)) ylim <- c(-resTh$prms$bnds - 20, resTh$prms$bnds + 20)

  # xaxis <- c(1:(resTh$prms$tmax)) - resTh$prms$drOnset
  # xlim  <- xlim - resTh$prms$drOnset
  xaxis <- c(1:(resTh$prms$tmax))

  # automatic
  plot(xaxis, resTh$sim$eq4, type = "l", lty = 2, col = cols,
       ylim = ylim, xlim = xlim,
       xlab = "", ylab = "",
       xaxt = xaxt, yaxt = yaxt, ...)
  title(xlab = xlab, ylab = ylab, line = xylabPos, ...)
  if (xaxt == "n") axis(side = 1, labels = FALSE)  # keep tick marks
  if (yaxt == "n") axis(side = 2, labels = FALSE)  # keep tick marks

  lines(xaxis, -resTh$sim$eq4, type = "l", lty = 2, col = cols[2], ...)

  # controlled
  if (resTh$prms$drDist != 0) {
    dr <- mean(c(resTh$prms$drLim1, resTh$prms$drLim2))
  } else {
    dr <- resTh$prms$drc
  }
  dr <- cumsum(rep(dr, resTh$prms$tmax))
  dr <- dr + mean(c(resTh$prms$spLim1, resTh$prms$spLim2))

  # adjust for controlled drift rate onset?
  if (resTh$prms$drOnset > 0) {
    dr <- dr - dr[resTh$prms$drOnset]
    dr[1:resTh$prms$drOnset] <- 0
  }

  lines(xaxis, dr, ...)

  # superimposed automatic + controlled comp/incomp
  lines(xaxis, resTh$sim$activation_comp,   col = cols[1], ...)
  lines(xaxis, resTh$sim$activation_incomp, col = cols[2], ...)

  # bounds
  lines(xaxis, resTh$sim$bnds, col = "darkgrey", xpd = FALSE, ...)
  lines(xaxis, -resTh$sim$bnds, col = "darkgrey", xpd = FALSE, ...)

  if (legend) {
    delfault_legend.parameters <- list(legend = labels, x = "bottomright",
                                       col = cols, lty = c(1,1), pch = c(0,0), inset = 0)
    legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
    add_legend(legend.parameters)
  }

}

plot_trials <- function(
    resTh,
    labels = c("Compatible", "Incompatible"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    ylab = "X(t)",
    xlim = NULL,
    ylim = NULL,
    xaxt = "s",
    yaxt = "s",
    xylabPos = 2,
    legend = TRUE,
    legend.parameters = list(),
    ...
) {

  if (is.null(resTh$trials)) {
    stop("plotting trials data requires dmcSim with fullData = TRUE")
  }

  if (is.null(xlim)) xlim <- c(0, resTh$prms$tmax)
  if (is.null(ylim)) ylim <- c(-resTh$prms$bnds - 20, resTh$prms$bnds + 20)

  plot(NULL, NULL,
       ylim = ylim, xlim = xlim,
       xlab = "", ylab = "",
       xaxt = xaxt, yaxt = yaxt, ...)
  title(xlab = xlab, ylab = ylab, line = xylabPos)
  if (xaxt == "n") axis(side = 1, labels = FALSE)  # keep tick marks
  if (yaxt == "n") axis(side = 2, labels = FALSE)  # keep tick marks

  # individual trials until bounds
  for (trl in c(1:resTh$prms$nTrlData)) {
    idx <- min(which(abs(resTh$trials$comp[[trl]]) >= resTh$sim$bnds)[1], length(resTh$trials$comp[[trl]]), na.rm = TRUE)
    lines(resTh$trials$comp[[trl]][1:idx], type = "l", col = cols[1], ...)
    idx <- min(which(abs(resTh$trials$incomp[[trl]]) >= resTh$sim$bnds)[1], length(resTh$trials$incomp[[trl]]), na.rm = TRUE)
    lines(resTh$trials$incomp[[trl]][1:idx], type = "l", col = cols[2], ...)
  }

  # bounds
  lines(resTh$sim$bnds, col = "darkgrey", xpd = FALSE, ...)
  lines(-resTh$sim$bnds, col = "darkgrey", xpd = FALSE, ...)

  if (legend) {
    delfault_legend.parameters <- list(legend = labels, x = "bottomright",
                                       col = cols, lty = c(1,1), pch = c(0,0), inset = 0)
    legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
    add_legend(legend.parameters)
  }

}

plot_pdf <- function(
    resTh,
    labels = c("Compatible", "Incompatible"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    ylab = "PDF",
    xlim = NULL,
    ylim = NULL,
    xaxt = "s",
    yaxt = "s",
    xylabPos = 2,
    legend = TRUE,
    legend.parameters = list(),
    ...
) {

  if (is.null(xlim)) xlim <- c(0, resTh$prms$tmax)
  if (is.null(ylim)) ylim <- c(0, 0.01)

  plot(density(resTh$sim$rts_comp), col = cols[1], main = NA, type = "l",
       ylim = ylim, xlim = xlim,
       ylab = "", xlab = "",
       xaxt = xaxt, yaxt = "n", ...)
  title(xlab = xlab, ylab = ylab, line = xylabPos)

  if (xaxt == "n") axis(side = 1, labels = FALSE)  # keep tick marks
  if (yaxt == "n") axis(side = 2, labels = FALSE)  # keep tick marks
  if (yaxt == "s") axis(side = 2, at = c(0, 0.005, 0.01), labels = c("0", ".005", ".001"))

  lines(density(resTh$sim$rts_incomp), col = cols[2], type = "l", ...)

  abline(h = 0, col = "darkgrey", lty = 2, xpd = FALSE);

  if (legend) {
    delfault_legend.parameters <- list(legend = labels, x = "topright",
                                       col = cols, lty = c(1,1), pch = c(NA,NA), inset = 0)
    legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
    add_legend(legend.parameters)
  }

}

plot_cdf <- function(
    resTh = NULL,
    resOb = NULL,
    labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    ylab = "PDF",
    xlim = NULL,
    xaxt = "s",
    yaxt = "s",
    xylabPos = 2,
    legend = TRUE,
    legend.parameters = list(),
    ...
) {

  datThCompX   <- NULL
  datThIncompX <- NULL
  datObCompX   <- NULL
  datObIncompX <- NULL

  if (!is.null(resTh) & is.null(resOb)) {
    densityComp   <- density(resTh$sim$rts_comp)
    datThCompX    <- densityComp$x
    datThCompY    <- cumsum(densityComp$y * diff(densityComp$x[1:2]))
    densityIncomp <- density(resTh$sim$rts_incomp)
    datThIncompX  <- densityIncomp$x
    datThIncompY  <- cumsum(densityIncomp$y * diff(densityIncomp$x[1:2]))
    types         <- c("l", "o")
    labels        <- labels[1:2]
  } else if (is.null(resTh) & !is.null(resOb)) {
    ndelta       <- nrow(resOb$delta)
    datObCompX   <- resOb$delta$meanComp
    datObCompY   <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    datObIncompX <- resOb$delta$meanIncomp
    datObIncompY <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    types        <- c("l", "o")
    labels       <- labels[1:2]
  } else if (!is.null(resTh) & !is.null(resOb)) {
    ndelta        <- nrow(resTh$delta)

    # densityComp   <- density(resTh$sim$rts_comp)
    # datThCompX    <- densityComp$x
    # datThCompY    <- cumsum(densityComp$y * diff(densityComp$x[1:2]))
    # densityIncomp <- density(resTh$sim$rts_incomp)
    # datThIncompX  <- densityIncomp$x
    # datThIncompY  <- cumsum(densityIncomp$y * diff(densityIncomp$x[1:2]))

    datThCompX   <- resTh$delta$meanComp
    datThCompY   <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    datThIncompX <- resTh$delta$meanIncomp
    datThIncompY <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]

    datObCompX   <- resOb$delta$meanComp
    datObCompY   <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    datObIncompX <- resOb$delta$meanIncomp
    datObIncompY <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    types        <- c("l", "p")
  }

  if (is.null(xlim)) {
    minx <- min(datObCompX, datThCompX, datObIncompX, datThIncompX)
    maxx <- max(datObCompX, datThCompX, datObIncompX, datThIncompX)
    xlim <- c(minx, maxx)
  }

  plot(NULL, NULL,
       ylab = "", xlab = "",
       ylim = c(0, 1), xlim = xlim,
       xaxt = xaxt, yaxt = "n")
  title(xlab = xlab, ylab = ylab, line = xylabPos)

  if (!is.null(datThCompX)) {
    lines(datThCompX,   datThCompY,   type = types[1], col = cols[1], ...)
    lines(datThIncompX, datThIncompY, type = types[1], col = cols[2], ...)
  }

  if (!is.null(datObCompX)) {
    lines(datObCompX,   datObCompY,   type = types[2], col = cols[1], ...)
    lines(datObIncompX, datObIncompY, type = types[2], col = cols[2], ...)
  }

  if (xaxt == "n") axis(side = 1, labels = FALSE)  # keep tick marks
  if (yaxt == "n") axis(side = 2, labels = FALSE)  # keep tick marks
  if (yaxt == "s") axis(side = 2, at = seq(0, 1, 0.5), labels = as.character(seq(0, 1, 0.5)))

  abline(h = c(0, 1), col = "darkgrey", lty = 2, xpd = FALSE);

  if (legend) {
    if (length(labels) == 4) {
      labels <- c(paste(labels[1], labels[3], sep = " "),
                  paste(labels[2], labels[3], sep = " "),
                  paste(labels[1], labels[4], sep = " "),
                  paste(labels[2], labels[4], sep = " "))
      delfault_legend.parameters <- list(legend = labels, x = "bottomright",
                                         col = cols, lty = c(0,0,1,1), pch = c(1,1,NA,NA), inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    } else {
      delfault_legend.parameters <- list(legend = labels, x = "bottomright",
                                         col = cols, lty = c(1,1), pch = c(NA,NA), inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }
  }

}


plot_caf <- function(
    resTh = NULL,
    resOb = NULL,
    labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
    cols = c("green", "red"),
    xlab = "RT Bin",
    ylab = "CAF",
    ylim = NULL,
    xaxt = "s",
    yaxt = "s",
    xylabPos = 2,
    cafBinLabels = FALSE,
    legend = TRUE,
    legend.parameters = list(),
    ...) {

  datObComp   <- NULL
  datObIncomp <- NULL
  datThComp   <- NULL
  datThIncomp <- NULL

  if (!is.null(resTh)) {
    datThComp   <- resTh$caf$accPerComp
    datThIncomp <- resTh$caf$accPerIncomp
    nCAF        <- length(datThComp)
  }
  if (!is.null(resOb)) {
    datObComp   <- resOb$caf$accPerComp
    datObIncomp <- resOb$caf$accPerIncomp
    nCAF        <- length(datObComp)
  }

  if (!is.null(resTh) & !is.null(resOb)) {
    types <- c("p", "l")
  } else {
    types <- c("o", "o")
    labels <- labels[1:2]
  }

  if (is.null(ylim)) ylim <- c(0, 1)

  plot(NULL, NULL, type = "o",
       ylim = ylim, xlim = c(1, nCAF),
       ylab = "",  xlab = "",
       xaxt = "n",  yaxt = "n",
       col = cols[1], ...)
  title(xlab = xlab, ylab = ylab, line = xylabPos)

  if (!is.null(datObComp)) {
    lines(datObComp,   type = types[1], col = cols[1], ...)
    lines(datObIncomp, type = types[1], col = cols[2], ...)
  }
  if (!is.null(datThComp)) {
    lines(datThComp,   type = types[2], col = cols[1], ...)
    lines(datThIncomp, type = types[2], col = cols[2], ...)
  }

  if (xaxt == "n") {
    axis(side = 1, labels = FALSE)  # keep tick marks
  } else if (xaxt == "s" | cafBinLabels) {
    if (cafBinLabels) {
      stepCAF <- 100 / nCAF
      cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
      axis(1, at = seq(1, nCAF, 1), labels = cafLabels)
    } else {
      axis(1, at = seq(1, nCAF, 1))
    }
  } else {
    axis(side = 1, labels = F)
  }

  if (yaxt == "n") {
    axis(side = 2, labels = FALSE)  # keep tick marks
  } else if (yaxt == "s") {
    axis(2, at = seq(ylim[1], ylim[2], 0.2), labels = as.character(seq(ylim[1], ylim[2], 0.2)))
  }

  if (legend) {
    if (length(labels) == 4) {
      labels <- c(paste(labels[1], labels[3], sep = " "),
                  paste(labels[2], labels[3], sep = " "),
                  paste(labels[1], labels[4], sep = " "),
                  paste(labels[2], labels[4], sep = " "))
      delfault_legend.parameters <- list(legend = labels, x = "bottomright",
                                         col = cols, lty = c(0,0,1,1), pch = c(1,1,NA,NA), inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    } else {
      delfault_legend.parameters <- list(legend = labels, x = "bottomright",
                                         col = cols, lty = c(1,1), pch = c(1,1), inset = 0)
      legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
      add_legend(legend.parameters)
    }
  }

}


plot_delta <- function(
    resTh = NULL,
    resOb = NULL,
    figType = "delta",
    labels = NULL,
    xlim = NULL,
    ylim = NULL,
    xlab = "Time [ms]",
    ylab = "Delta [ms]",
    xaxt = "s",
    yaxt = "s",
    xylabPos = 2,
    legend = TRUE,
    legend.parameters = list(),
    ...)
{

  datObX <- NULL
  datObY <- NULL
  datThX <- NULL
  datThY <- NULL

  figType <- tolower(figType)
  if (figType == "delta") {
    if (!is.null(resTh)) {
      datThX <- resTh$delta$meanBin
      datThY <- resTh$delta$meanEffect
    }
    if (!is.null(resOb)) {
      datObX <- resOb$delta$meanBin
      datObY <- resOb$delta$meanEffect
    }
  } else if (figType == "deltaerrors") {
    if (!is.null(resTh)) {
      datThX <- resTh$deltaErrors$meanBin
      datThY <- resTh$deltaErrors$meanEffect
    }
    if (!is.null(resOb)) {
      datObX <- resOb$deltaErrors$meanBin
      datObY <- resOb$deltaErrors$meanEffect
    }
  }

  if (!is.null(datObX) & !is.null(datThX)) {
    types <- c("p", "l")
  } else {
    types <- c("o", "o")
  }

  if (is.null(xlim)) {
    xlim <- c(min(datObX, datThX) - 20, max(datObX, datThX) + 20)
  }
  if (is.null(ylim)) {
    ylim <- c(min(datObY, datThY) - 20, max(datObY, datThY) + 20)
  }

  if (any(is.na(xlim))) xlim <- NULL
  if (any(is.na(ylim))) ylim <- NULL

  plot(NULL, NULL,
       ylim = ylim, xlim = xlim,
       ylab = "",  xlab = "",
       xaxt = xaxt, yaxt = yaxt, ...)
  title(xlab = xlab, ylab = ylab, line = xylabPos)
  axis(side = 1, labels = FALSE)
  axis(side = 2, labels = FALSE)

  if (!is.null(datThX)) {
    lines(datThX, datThY, type = types[2], ...)
  }
  if (!is.null(datObX)) {
    lines(datObX, datObY, type = types[1], ...)
  }

  if (!is.null(labels) && legend) {
    delfault_legend.parameters <- list(legend = labels, x = "bottomright",
                                       col = "black", lty = c(0,1), pch = c(1,NA), inset = 0)
    legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
    add_legend(legend.parameters)
  }

}

plot_delta_er <- function(
    resTh = NULL,
    resOb = NULL,
    labels = NULL,
    xlim = NULL,
    ylim = NULL,
    xlab = "Time [ms]",
    ylab = "Delta [%]",
    xaxt = "s",
    yaxt = "s",
    xylabPos = 2,
    legend = TRUE,
    legend.parameters = list(),
    ...)
{

  datObX <- NULL
  datObY <- NULL
  datThX <- NULL
  datThY <- NULL

  if (!is.null(resTh)) {
     datThX <- resTh$caf$meanBin
     datThY <- resTh$caf$meanEffect
  }
  if (!is.null(resOb)) {
    datObX <- resOb$caf$meanBin
    datObY <- resOb$caf$meanEffect
  }

  if (!is.null(datObX) & !is.null(datThX)) {
    types <- c("p", "l")
  } else {
    types <- c("o", "o")
  }

  if (is.null(xlim)) {
    xlim <- c(min(datObX, datThX) - 2, max(datObX, datThX) + 2)
  }
  if (is.null(ylim)) {
    ylim <- c(min(datObY, datThY) - 2, max(datObY, datThY) + 2)
  }

  if (any(is.na(xlim))) xlim <- NULL
  if (any(is.na(ylim))) ylim <- NULL

  plot(NULL, NULL,
       ylim = ylim, xlim = xlim,
       ylab = "",  xlab = "",
       xaxt = xaxt, yaxt = yaxt, ...)
  title(xlab = xlab, ylab = ylab, line = xylabPos)
  axis(side = 1, labels = FALSE)
  axis(side = 2, labels = FALSE)

  if (!is.null(datThX)) {
    lines(datThX, datThY, type = types[2], ...)
  }
  if (!is.null(datObX)) {
    lines(datObX, datObY, type = types[1], ...)
  }

  if (!is.null(labels) && legend) {
    delfault_legend.parameters <- list(legend = labels, x = "bottomright",
                                       col = "black", lty = c(0,1), pch = c(1,NA), inset = 0)
    legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
    add_legend(legend.parameters)
  }

}


plot_beh <- function(
    resTh = NULL,
    resOb = NULL,
    figType = "rtcorrect",
    xlabs = c("Compatible", "Incompatible"),
    ylab = NULL,
    ylim = NULL,
    condLabels = NULL,
    yaxt = "s",
    xylabPos = 2,
    legend = TRUE,
    legend.parameters = list(),
    ...)
{

  datOb <- NULL
  datTh <- NULL
  if (figType == "rtcorrect") {
    if (!is.null(resTh)) datTh <- resTh$summary$rtCor
    if (!is.null(resOb)) datOb <- resOb$summary$rtCor
    if (is.null(ylim)) {
      ylim <- c(min(datOb, datTh, na.rm = TRUE) * 0.9, max(datOb, datTh, na.rm = TRUE) * 1.1)
    }
    if (is.null(ylab)) ylab <- "RT Correct [ms]"
    legendPosition = "topleft"
  } else if (figType == "rterrors") {
    if (!is.null(resTh)) datTh <- resTh$summary$rtErr
    if (!is.null(datTh) & all(is.na(datTh))) datTh <- NULL
    if (!is.null(resOb)) datOb <- resOb$summary$rtErr
    if (!is.null(datOb) & all(is.na(datOb))) datOb <- NULL
    if (is.null(ylim)) {
      ylim = c(0, 0)
      if (!is.null(datOb) | !is.null(datTh)) {
        ylim <- c(min(datOb, datTh, na.rm = TRUE) * 0.9, max(datOb, datTh, na.rm = TRUE) * 1.1)
      }
    }
    if (is.null(ylab)) ylab <- "RT Error [ms]"
    legendPosition = "topright"
  } else if (figType == "errorrate") {
    if (!is.null(resTh)) datTh <- resTh$summary$perErr
    if (!is.null(resOb)) datOb <- resOb$summary$perErr
    if (is.null(ylim)) ylim <- c(0, max(datOb, datTh, na.rm = TRUE) * 1.5)
    if (is.null(ylab)) ylab <- "Error Rate [%]"
    legendPosition = "topleft"
  }

  if (!is.null(resTh) & !is.null(resOb) & is.null(condLabels)) {
    condLabels <- c("Observed", "Predicted")
  }

  plot(NULL, NULL,
       ylim = ylim, xlim = c(0.5, 2.5),
       ylab = "", xlab = "",
       xaxt = "n",  yaxt = yaxt, ...)
  title(ylab = ylab, line = xylabPos)
  axis(1, at = c(1, 2), labels = xlabs)
  axis(2, labels = FALSE)

  ltys <- c(1, 2)
  pchs <- c(1, 1)

  if (!is.null(datOb)) {
    lines(c(1, 2), datOb, type = "o", col = "black", pch = pchs[1], lty = ltys[1])
  }

  if (!is.null(datTh)) {
    lines(c(1, 2), datTh, type = "o", col = "black", pch = pchs[2], lty = ltys[2])
  }

  if (!is.null(condLabels) & legend) {
    delfault_legend.parameters <- list(legend = labels, x = legendPosition,
                                       col = "black", lty = ltys, pch = pchs, inset = 0)
    legend.parameters <- modifyList(delfault_legend.parameters, legend.parameters)
    add_legend(legend.parameters)
  }

}

plot_distribution <- function(
    resTh,
    labels = c("Compatible", "Incompatible"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    xlim = NULL
) {

  # keep original user par and reset later
  opar <- par(no.readonly = TRUE)

  # histogram of RT distributions
  par(mfrow = (c(2, 1)))
  par(mar = c(0, 4, 2, 2))

  # data
  comp_correct   <- resTh$sim$rts_comp
  incomp_correct <- resTh$sim$rts_incomp
  comp_error     <- resTh$sim$errs_comp
  incomp_error   <- resTh$sim$errs_incomp
  y              <- length(comp_correct) / 10

  if (is.null(xlim)) {
    xlim <- c(min(comp_correct, incomp_correct, comp_error, incomp_error),
              c(max(comp_correct, incomp_correct, comp_error, incomp_error)))
  }

  hist(comp_correct,
       xlim = xlim, ylim = c(0, y),
       xaxt = "n", col = scales::alpha(cols[1], .5), border = FALSE,
       breaks = 100, main = "", yaxt = "n", xlab = "", ylab = "")
  abline(v = mean(comp_correct), col = cols[1], lwd = 2, xpd = FALSE)
  legend("topright", labels, fill = cols, bty = "n", cex = 2)

  hist(incomp_correct,  add = TRUE,
       xlim = xlim, ylim = c(0, y),
       xaxt = "n", col = scales::alpha(cols[2], .5),
       border = FALSE, breaks = 100, main = "", xlab = "", ylab = "")
  abline(v = mean(incomp_correct), col = cols[2], lwd = 2, xpd = FALSE)

  # Error RTs
  par(mar = c(5, 4, 0, 2))

  if (length(comp_error) > 0) {
    hist(comp_error,
         xlim = xlim, ylim = c(y, 0),
         col = scales::alpha(cols[1], .5), border = FALSE,
         breaks = 100, main = "", yaxt = "n", xlab = xlab, ylab = "", cex.axis = 1.5, cex.lab = 2)
    abline(v = mean(comp_error), col = cols[1], lwd = 2, xpd = FALSE)
  }

  if (length(incomp_error) > 0) {
    hist(incomp_error, add = TRUE,
         xlim = xlim, ylim = c(y, 0),
         col = scales::alpha(cols[2], .5),
         border = FALSE, breaks = 100, main = "", ylab = "")
    abline(v = mean(incomp_error), col = cols[2], lwd = 2, xpd = FALSE)
  }

  # reset original par
  par(opar)

}

add_legend <- function(legend.parameters) {
  do.call(legend, legend.parameters)
}


########################### ggplot2 #######################################
plot_theme_ggplot2 <- function() {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      plot.margin      = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  return(theme)
}

plot_activation_ggplot2 <- function(
    resTh,
    labels = c("Compatible", "Incompatible"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    ylab = "E[X(t)]",
    xlim = NULL,
    ylim = NULL,
    legendPosition = "bottom",
    theme = NULL
) {

  if (!requireNamespace("ggplot2")) {
    stop("This addin requires the 'ggplot2' package.")
  }

  if (is.null(ylim)) {
    ylim <- c(-resTh$prms$bnds - 20, resTh$prms$bnds + 20)
  }
  if (is.null(xlim)) {
    xlim <- c(0, resTh$prms$tmax)
  }

  dr <- resTh$prms$drc
  dr <- cumsum(rep(dr, resTh$prms$tmax))
  dr <- dr + mean(c(resTh$prms$spLim1, resTh$prms$spLim2))

  dat <- data.frame(
    time   = 1:resTh$prms$tmax,
    dr     = dr,
    comp   = resTh$sim$activation_comp,
    incomp = resTh$sim$activation_incomp
  )

  plt <- ggplot2::ggplot(dat, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y =  resTh$sim$eq4, colour = labels[1]), linetype = 2, na.rm = TRUE)  +
    ggplot2::geom_line(ggplot2::aes(y = -resTh$sim$eq4, color = labels[2]),  linetype = 2, na.rm = TRUE)  +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::geom_line(ggplot2::aes(y = dr), na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = comp, color = labels[1]), na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = incomp, color = labels[2]),  na.rm = TRUE) +
    ggplot2::coord_cartesian( xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = xlab, y = ylab, color = "") +
    ggplot2::geom_hline(yintercept = c(-resTh$prms$bnds, resTh$prms$bnds), linetype = "dashed",  color = "darkgrey", size = 0.5) +
    plot_theme_ggplot2() +
    ggplot2::theme(legend.position = legendPosition)

  if (!is.null(theme)) {
    plt <- plt + theme
  }

  return(plt)

}


plot_trials_ggplot2 <- function(
    resTh,
    labels = c("Compatible", "Incompatible"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    ylab = "X(t)",
    xlim = NULL,
    ylim = NULL,
    legendPosition = "bottom",
    theme = NULL
) {

  if (is.null(resTh$trials)) {
    stop("plotting trials data requires dmcSim with fullData = TRUE")
  }

  if (!requireNamespace("ggplot2")) {
    stop("This addin requires the 'ggplot2' package.")
  }

  if (is.null(ylim)) {
    ylim <- c(-resTh$prms$bnds - 20, resTh$prms$bnds + 20)
  }
  if (is.null(xlim)) {
    xlim <- c(0, resTh$prms$tmax)
  }

  dat = data.frame(Trial = NULL, time = NULL, comp = NULL, data = NULL)
  for (trl in c(1:resTh$prms$nTrlData)) {
    idx <- min(which(abs(resTh$trials$comp[[trl]]) >= resTh$prms$bnds)[1], length(resTh$trials$comp[[trl]]), na.rm = TRUE)
    dat <- rbind(dat, data.frame(Trial = trl, time = 1:idx, comp = labels[1], data = resTh$trials$comp[[trl]][1:idx]))
    idx <- min(which(abs(resTh$trials$incomp[[trl]]) >= resTh$prms$bnds)[1], length(resTh$trials$incomp[[trl]]), na.rm = TRUE)
    dat <- rbind(dat, data.frame(Trial = -trl, time = 1:idx, comp = labels[2], data = resTh$trials$incomp[[trl]][1:idx]))
  }

  plt <- ggplot2::ggplot(dat, ggplot2::aes(x = time, y = data, group = Trial, color = comp)) +
    ggplot2::geom_line(ggplot2::aes(group = Trial, color = comp), size = 0.5, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::coord_cartesian( xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = xlab, y = ylab, color = "") +
    ggplot2::geom_hline(yintercept = c(-resTh$prms$bnds, resTh$prms$bnds), linetype = "dashed",  color = "darkgrey", size = 0.5) +
    plot_theme_ggplot2() +
    ggplot2::theme(legend.position = legendPosition)

  if (!is.null(theme)) {
    plt <- plt + theme
  }

  return(plt)

}

plot_pdf_ggplot2 <- function(
    resTh,
    labels = c("Compatible", "Incompatible"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    ylab = "PDF",
    xlim = NULL,
    ylim = NULL,
    legendPosition = "bottom",
    theme = NULL
) {

  if (!requireNamespace("ggplot2")) {
    stop("This addin requires the 'ggplot2' package.")
  }

  if (is.null(xlim)) {
    xlim <- c(0, resTh$prms$tmax)
  }

  comp <- rep(labels,  times = c(length(resTh$sim$rts_comp), length(resTh$sim$rts_incomp)))
  dat  <- data.frame(comp = comp, data = c(resTh$sim$rts_comp, resTh$sim$rts_incomp))

  plt <- ggplot2::ggplot(dat, ggplot2::aes(x = data, color = comp)) +
    ggplot2::stat_density(geom = "line", position = "identity", na.rm = TRUE) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::coord_cartesian( xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = xlab, y = ylab, color = "") +
    plot_theme_ggplot2() +
    ggplot2::theme(legend.position = legendPosition)

  if (!is.null(theme)) {
    plt <- plt + theme
  }

  return(plt)

}

plot_cdf_ggplot2 <- function(
    resTh=NULL,
    resOb=NULL,
    labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
    cols = c("green", "red"),
    xlab = "Time [ms]",
    ylab = "CDF",
    xlim = NULL,
    ylim = NULL,
    legendPosition = "bottom",
    theme = NULL
) {

  if (!requireNamespace("ggplot2")) {
    stop("This addin requires the 'ggplot2' package.")
  }

  datThCompX   <- NULL
  datThIncompX <- NULL
  datObCompX   <- NULL
  datObIncompX <- NULL
  dat          <- NULL
  datOb        <- NULL
  datTh        <- NULL

  if (!is.null(resTh) & is.null(resOb)) {
    densityComp   <- density(resTh$sim$rts_comp)
    datThCompX    <- densityComp$x
    datThCompY    <- cumsum(densityComp$y * diff(densityComp$x[1:2]))
    densityIncomp <- density(resTh$sim$rts_incomp)
    datThIncompX  <- densityIncomp$x
    datThIncompY  <- cumsum(densityIncomp$y * diff(densityIncomp$x[1:2]))

    comp <- rep(labels[1:2], times = c(length(datThCompY), length(datThIncompY)))
    dat  <- data.frame(comp = comp, time = c(datObCompX, datThIncompX), pdf = c(datThCompY, datThIncompY))

  } else if (is.null(resTh) & !is.null(resOb)) {
    ndelta       <- nrow(resOb$delta)
    datObCompX   <- resOb$delta$meanComp
    datObCompY   <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    datObIncompX <- resOb$delta$meanIncomp
    datObIncompY <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]

    comp <- rep(labels[1:2], times = c(length(datObCompY), length(datObIncompY)))
    dat  <- data.frame(comp = comp, time = c(datObCompX, datObIncompX), pdf = c(datObCompY, datObIncompY))

  } else if (!is.null(resTh) & !is.null(resOb)) {

    ndelta       <- nrow(resOb$delta)
    datObCompX   <- resOb$delta$meanComp
    datObCompY   <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    datObIncompX <- resOb$delta$meanIncomp
    datObIncompY <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]

    labels <- c(paste(labels[1], labels[3], sep = " "),
                paste(labels[2], labels[3], sep = " "),
                paste(labels[1], labels[4], sep = " "),
                paste(labels[2], labels[4], sep = " "))

    comp   <- rep(labels[1:2], times = c(length(datObCompY), length(datObIncompY)))
    datOb  <- data.frame(comp = comp, time = c(datObCompX, datObIncompX), pdf = c(datObCompY, datObIncompY))

    ndelta       <- nrow(resTh$delta)
    datThCompX   <- resTh$delta$meanComp
    datThCompY   <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]
    datThIncompX <- resTh$delta$meanIncomp
    datThIncompY <- seq(0, 1, length.out = ndelta + 2)[2:(ndelta + 1)]

    comp  <- rep(labels[3:4], times = c(length(datThCompY), length(datThIncompY)))
    datTh <- data.frame(comp = comp, time = c(datThCompX, datThIncompX), pdf = c(datThCompY, datThIncompY))

  }

  if (is.null(xlim)) {
    minx <- min(datObCompX, datThCompX, datObIncompX, datThIncompX)
    maxx <- max(datObCompX, datThCompX, datObIncompX, datThIncompX)
    xlim <- c(minx, maxx)
  }

  if (!is.null(dat)) {

    plt <- ggplot2::ggplot(dat, ggplot2::aes(x = time, y = pdf, color = comp)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::coord_cartesian( xlim = xlim, ylim = ylim) +
      ggplot2::labs(x = xlab, y = ylab, color = "") +
      ggplot2::geom_hline(yintercept = c(0, 1), linetype = "dashed",  color = "darkgrey", size = 0.5)
    if (!is.null(resOb)) {
      plt <- plt + ggplot2::geom_point()
    }

  } else {

    plt <- ggplot2::ggplot(NULL, ggplot2::aes(x = time, y = pdf)) +
      ggplot2::geom_point(data = datOb, ggplot2::aes(group = 1, color = comp)) +
      ggplot2::geom_line(data = datTh, ggplot2::aes(color = comp), show.legend = TRUE) +
      ggplot2::scale_color_manual(name = "", values = rep(cols, times = 2), labels = labels,
                                  breaks = c(labels[3], labels[4], labels[1], labels[2]),
                                  guide = ggplot2::guide_legend(nrow = 2, byrow = TRUE, override.aes = list(
                                    linetype = c("blank", "blank", "solid", "solid"),
                                    shape = c(16, 16, NA, NA)))
      ) +
      ggplot2::coord_cartesian( xlim = xlim, ylim = ylim) +
      ggplot2::labs(x = xlab, y = ylab, color = "")
  }

  plt <- plt +
    plot_theme_ggplot2() +
    ggplot2::theme(legend.position = legendPosition)
  if (!is.null(theme)) {
    plt <- plt + theme
  }

  return(plt)

}

plot_caf_ggplot2 <- function(
    resTh=NULL,
    resOb=NULL,
    labels = c("Compatible", "Incompatible", "Observed", "Predicted"),
    cols = c("green", "red"),
    xlab = "Bin",
    ylab = "CAF",
    ylim = c(0, 1),
    legendPosition = "bottom",
    theme = NULL
) {

  if (!requireNamespace("ggplot2")) {
    stop("This addin requires the 'ggplot2' package.")
  }

  if (!is.null(resTh) & !is.null(resOb)) {
    types <- c("p", "l")
  } else {
    types <- c("o", "o")
    labels <- labels[1:2]
  }

  dat <- NULL
  if (!is.null(resTh) & is.null(resOb)) {
    comp <- rep(labels, each = length(resTh$caf$accPerComp))
    dat  <- data.frame(comp = comp, bin = 1:length(resTh$caf$accPerComp), data = c(resTh$caf$accPerComp, resTh$caf$accPerIncomp))
  } else if (is.null(resTh) & !is.null(resOb)) {
    comp <- rep(labels, each = length(resOb$caf$accPerComp))
    dat  <- data.frame(comp = comp, bin = 1:length(resOb$caf$accPerComp), data = c(resOb$caf$accPerComp, resOb$caf$accPerIncomp))
  } else if (!is.null(resTh) & !is.null(resOb)) {
    labels <- c(paste(labels[1], labels[3], sep = " "),
                paste(labels[2], labels[3], sep = " "),
                paste(labels[1], labels[4], sep = " "),
                paste(labels[2], labels[4], sep = " "))
    comp  <- rep(labels[1:2], each = length(resOb$caf$accPerComp))
    datOb <- data.frame(comp = comp, bin = 1:length(resOb$caf$accPerComp), data = c(resOb$caf$accPerComp, resOb$caf$accPerIncomp))
    comp  <- rep(labels[3:4], each = length(resTh$caf$accPerComp))
    datTh <- data.frame(comp = comp, bin = 1:length(resTh$caf$accPerComp), data = c(resTh$caf$accPerComp, resTh$caf$accPerIncomp))
  }

  if (!is.null(dat)) {

    plt <- ggplot2::ggplot(dat, ggplot2::aes(x = bin, y = data, color = comp)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::coord_cartesian(ylim = ylim) +
      ggplot2::labs(x = xlab, y = ylab, color = "")

  } else {

    plt <- ggplot2::ggplot(NULL, ggplot2::aes(x = bin, y = data)) +
      ggplot2::geom_point(data = datOb, ggplot2::aes(group = 1, color = comp)) +
      ggplot2::geom_line(data = datTh, ggplot2::aes(color = comp), show.legend = TRUE) +
      ggplot2::scale_color_manual(name = "", values = rep(cols, each = 2), labels = labels,
                                  guide = ggplot2::guide_legend(nrow = 2, byrow = TRUE, override.aes = list(
                                    color = c(cols[1], cols[2], cols[1], cols[2]),
                                    linetype = c("blank", "blank", "solid", "solid"),
                                    shape = c(16, 16, NA, NA)))
      ) +
      ggplot2::coord_cartesian(ylim = ylim) +
      ggplot2::labs(x = xlab, y = ylab, color = "")

  }

  plt <- plt +
  plot_theme_ggplot2() +
  ggplot2::theme(legend.position = legendPosition)
  if (!is.null(theme)) {
    plt <- plt + theme
  }

  return(plt)

}

plot_delta_ggplot2 <- function(
    resTh,
    xlab = "Time [ms]",
    ylab = "Delta [ms]",
    xlim = NULL,
    ylim = NULL,
    legendPosition = "bottom",
    theme = NULL
) {

  if (!requireNamespace("ggplot2")) {
    stop("This addin requires the 'ggplot2' package.")
  }

  dat  <- data.frame(time = resTh$delta$meanBin, effect = resTh$delta$meanEffect)

  plt <- ggplot2::ggplot(dat, ggplot2::aes(x = time, y = effect)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = xlab, y = ylab) +
    plot_theme_ggplot2() +
    ggplot2::theme(legend.position = legendPosition)

  if (!is.null(theme)) {
    plt <- plt + theme
  }

  return(plt)

}

plot_deltas_ggplot2 <- function(
    resTh,
    xlab = "Time [ms]",
    ylab = "Delta [ms]",
    col = c("black", "lightgrey"),
    xlim = NULL,
    ylim = NULL,
    ncol = 1,
    legendPosition = "right",
    theme = NULL
) {

  # colour range
  cols <- colorRampPalette(col)(length(resTh))
  legendLabels = NULL
  for (i in seq_along(resTh)) {
    legendLabels <- rbind(legendLabels, paste0(names(resTh[[i]]$params), "=", resTh[[1]]$params[i,], collapse = ", "))
  }
  dat <- data.frame(Parameters = legendLabels[1], resTh[[1]]$delta)
  for (i in c(2:length(resTh))) {
    dat <- rbind(dat, data.frame(Parameters = legendLabels[i], resTh[[i]]$delta))
  }

  plt <- ggplot2::ggplot(dat, ggplot2::aes(x = meanBin, y = meanEffect, color = Parameters)) +
    ggplot2::geom_line(ggplot2::aes(group = Parameters) )  +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = ncol)) +
    plot_theme_ggplot2() +
    ggplot2::theme(legend.position = legendPosition)

  return(plt)

}


plot_beh_ggplot2 <- function(resTh = NULL,
                             resOb = NULL,
                             figType = "rtcorrect",
                             xlabs = c("Compatible", "Incompatible"),
                             ylab = NULL,
                             ylim = NULL,
                             legend = TRUE,
                             condLabels = NULL,
                             legendPosition = "bottom",
                             theme=NULL,
                             ...) {

  if (figType == "rtcorrect") figType <- "rtCor"
  if (figType == "rterrors") figType <- "rtErr"
  if (figType == "errorrate") figType <- "perErr"

  datTh <- NULL
  datOb <- NULL
  if (!is.null(resTh)) datTh <- resTh$summary[, figType]
  if (!is.null(resOb)) datOb <- resOb$summary[, figType]

  if (is.null(ylab)) {
    if (figType == "rtCor") ylab <- "RT Correct [ms]"
    if (figType == "rtErr") ylab <- "RT Error [ms]"
    if (figType == "perErr") ylab <- "Error Rate [%]"
  }

  if (is.null(ylim)) {
    if (figType %in% c("rtCor", "rtErr")) {
      ylim <- c(min(datOb, datTh, na.rm = TRUE) * 0.9, max(datOb, datTh, na.rm = TRUE) * 1.1)
    } else {
      ylim <- c(0, max(datOb, datTh, na.rm = TRUE) * 1.5)
    }
  }

  if (is.null(resTh) | is.null(resOb)) {

    if (is.null(resTh)) {
      y <- datOb
    } else {
      y <- datTh
    }
    dat <- data.frame(x = xlabs, y = y)

    plt <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(group = 1) +
      ggplot2::geom_point(group = 1)  +
      ggplot2::labs(x = "", y = ylab) +
      ggplot2::coord_cartesian(ylim = ylim) +
      plot_theme_ggplot2()

  } else if (!is.null(resTh) & !is.null(resOb)) {

    if (is.null(condLabels)) {
      condLabels <- c("Observed", "Predicted")
    }
    dat <- data.frame(cond = rep(condLabels, each = 2), x = xlabs, y = c(datOb, datTh))

    plt <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y, linetype = cond)) +
      ggplot2::geom_line(ggplot2::aes(group = cond)) +
      ggplot2::geom_point(ggplot2::aes(group = cond))  +
      ggplot2::labs(x = "", y = ylab) +
      ggplot2::coord_cartesian(ylim = ylim) +
      plot_theme_ggplot2() +
      ggplot2::theme(legend.position = legendPosition,
                     legend.title = ggplot2::element_blank())

  }

  if (!is.null(theme)) {
    plt <- plt + theme
  }

  return(plt)

}
