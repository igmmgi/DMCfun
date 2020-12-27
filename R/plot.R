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
#' @param figType summary1, summary2, summary3, activation, trials, pdf, cdf, caf, delta, rtCorrect, rtErrors, errorRate, all
#' @param ylimCAF ylimit for CAF plot
#' @param cafBinLabels TRUE/FALSE
#' @param ylimDelta ylimit for delta plot
#' @param xlimDelta xlimit for delta plot (Default is 0 to tmax)
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
#' @return NULL
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
                        ylimCAF = c(0, 1),
                        cafBinLabels = FALSE,
                        ylimDelta = c(-50, 150),
                        xlimDelta = NULL,
                        ylimRt = c(200, 800),
                        ylimErr = c(0, 20),
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
  
  figTypes <- c("summary1", "summary2", "summary3", "all", "activation", "trials", "pdf", "cdf", "caf", "delta", "rtCorrect", "errorRate",  "rtErrors")
  if (length(figType) > 1 || !figType %in% figTypes) {
    stop("figType must be one of:", paste0(figTypes, collapse = ", "))
  }
  if ("summary1" %in% figType  & !("trials" %in% names(x))) {
    figType = "summary2"
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
  
  showFig <- rep(FALSE, 9)
  
  # xlabels
  if (xlabs) {
    xlabs           <- rep("", 9)
    xlabs[c(1:4,6)] <- c("Time [ms]")
    xlabs[c(5)]     <- c("RT Bin")
    xlabs[c(8:9)]   <- c(labels)
  } else {
    xlabs <- rep("", 9)
  }
  
  # y-labels
  if (ylabs) {
    ylabs <- c("E[X(t)]", "X(t)", "PDF", "CDF", "CAF", expression(Delta), "RT Correct [ms]", "Error Rate [%]", "RT Error [%]")
  } else {
    ylabs <- rep("", 9)
  }
  
  # x-axts & y-axts
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
    plot(c(1:x$prms$tmax), x$sim$eq4, type = "l", lty = 2, col = tail(cols, 2)[1],
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlab = xlabs[1], ylab = ylabs[1],
         xaxt = xaxts, yaxt = yaxts, ...)
    if (xaxts == "n") axis(side=1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") axis(side=2, labels = FALSE)  # keep tick marks
    
    lines(c(1:x$prms$tmax), -x$sim$eq4, type = "l", lty = 2, col = tail(cols, 2)[2], ...)
    
    # controlled
    dr <- ifelse(x$prms$varDR, mean(as.numeric(as.character(x$prms$drLim)[2:3])), x$prms$drc) 
    dr <- cumsum(rep(dr, x$prms$tmax))
    dr <- dr + mean(x$prms$spLim)
    lines(c(1:x$prms$tmax), dr, ...)
    
    # superimposed automatic + controlled comp/incomp
    lines(c(1:x$prms$tmax), x$sim$activation_comp,   col = tail(cols, 2)[1], ...)
    lines(c(1:x$prms$tmax), x$sim$activation_incomp, col = tail(cols, 2)[2], ...)
    
    # bounds
    lines(c(0, x$prms$tmax), c( x$prms$bnds,  x$prms$bnds), type = "l", col = "darkgrey", ...)
    lines(c(0, x$prms$tmax), c(-x$prms$bnds, -x$prms$bnds), type = "l", col = "darkgrey", ...)
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", legend = labels, col = tail(cols, 2), lty = c(1, 1), inset = c(0.05, 0.2))
    }
    
  }
  
  # individual trials
  if (showFig[2]) {
    
    # bounds
    plot(c(0, x$prms$tmax), c(x$prms$bnds, x$prms$bnds), type = "l", col = "darkgrey",
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlab = xlabs[2], ylab = ylabs[2],
         xaxt = xaxts, yaxt = yaxts, ...)
    if (xaxts == "n") axis(side=1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") axis(side=2, labels = FALSE)  # keep tick marks
    
    lines(c(0, x$prms$tmax), c(-x$prms$bnds, -x$prms$bnds), type = "l", col = "darkgrey", ...)
    
    # individual trials until bounds
    for (trl in c(1:x$prms$nTrlData)) {
      idx <- which(abs(x$trials$comp[[trl]]) >= x$prms$bnds)[1]
      lines(x$trials$comp[[trl]][1:idx], type = "l", col = tail(cols, 2)[1], ...)
      idx <- which(abs(x$trials$incomp[[trl]]) >= x$prms$bnds)[1]
      lines(x$trials$incomp[[trl]][1:idx], type = "l", col = tail(cols, 2)[2], ...)
    }
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", legend = labels, col = tail(cols, 2), lty = c(1, 1), inset = c(0.05, 0.2))
    }
    
  }
  
  # PDF
  if (showFig[3]) {
    
    plot(density(x$sim$rts_comp), col = tail(cols, 2)[1], main = NA, type = "l",
         ylim = c(0, 0.01), xlim = c(0, x$prms$tmax),
         ylab = ylabs[3], xlab = xlabs[3], xaxt = xaxts, yaxt = "n", ...)
    
    if (xaxts == "n") axis(side=1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side=2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(side = 2, at = c(0, 0.005, 0.01), labels = c("0", ".005", ".001"))
    }
    
    lines(density(x$sim$rts_incomp), col = tail(cols, 2)[2], type = "l", ...)
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("topright", legend = labels, col = tail(cols, 2), lty = c(1, 1), inset = c(0.05, 0.05))
    }
    
  }
  
  # CDF
  if (showFig[4]) {
    
    density_comp   <- density(x$sim$rts_comp)
    cdf_comp       <- cumsum(density_comp$y * diff(density_comp$x[1:2]))
    density_incomp <- density(x$sim$rts_incomp)
    cdf_incomp     <- cumsum(density_incomp$y * diff(density_incomp$x[1:2]))
    
    plot(density_comp$x, cdf_comp, type="l", col = tail(cols, 2)[1],
         ylab = ylabs[4], xlab = xlabs[4],
         ylim = c(0, 1), xaxt = xaxts, yaxt = "n", xlim = c(0, x$prms$tmax))
    
    if (xaxts == "n") axis(side=1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side=2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(side = 2, at = seq(0, 1, 0.5), labels = as.character(seq(0, 1, 0.5)))
    }
    
    lines(density_incomp$x, cdf_incomp, type = "l", col = tail(cols, 2)[2])
    abline(h = 0, lty = 2); abline(h = 1, lty = 2)
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", legend = labels, col = tail(cols, 2), lty = c(1, 1), inset = c(0.05, 0.05))
    }
    
  }
  
  # CAF
  if (showFig[5]) {
    
    plot(x$caf$accPer[x$caf$Comp == "comp"], type = "o",
         ylim = ylimCAF,
         ylab = ylabs[5],  xlab = xlabs[5],
         xaxt = "n",  yaxt = "n",
         col = tail(cols, 2)[1], ...)
    
    if (xaxts == "n") {
      axis(side=1, labels = FALSE)  # keep tick marks
    } else if (xaxts == "s" | cafBinLabels) {
      nCAF <- length(x$caf$bin) / 2
      if (cafBinLabels) {
        stepCAF <- 100 / nCAF
        cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
        axis(1, at = seq(1, nCAF, 1), labels = cafLabels, ...)
      } else {
        axis(1, at = seq(1, nCAF, 1), ...)
      }
    } else {
      axis(side=1,labels=F) 
    }
    
    if (yaxts == "n") {
      axis(side=2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)), ...)
    }
    
    lines(x$caf$accPer[x$caf$Comp == "incomp"], col = tail(cols, 2)[2], type = "o", ...)
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", legend = labels, col = tail(cols, 2), lty = c(1, 1), inset = c(0.05, 0.05))
    }
    
  }
  
  # delta
  if (showFig[6]) {
    
    if (is.null(xlimDelta)) { 
      xlimDelta <- c(0, x$prms$tmax)
    }
    
    plot(x$delta$meanBin, x$delta$meanEffect, type = "o", col = cols[1],
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = ylabs[6],  xlab = xlabs[6],
         xaxt = xaxts, yaxt = yaxts, ...)
    axis(side=1,labels = FALSE) 
    axis(side=2,labels = FALSE) 
  }
  
  # rtCorrect
  if (showFig[7]) {
    plot(c(1, 2), x$summary$rtCor, type = "o", col = cols[1],
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[7], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[8:9])
    axis(2, labels = FALSE)
    
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$rtCor, x$summary$sdRtCor)
    }
    
  }
  
  # error rate
  if (showFig[8]) {
    plot(c(1, 2), x$summary$perErr, type = "o", col = cols[1],
         ylim = ylimErr, xlim = c(0.5, 2.5),
         ylab = ylabs[8], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[8:9])
    axis(2, labels = FALSE)
  }
  
  # rtError
  if (showFig[9]) {
    plot(c(1, 2), x$summary$rtErr, type = "o", col = cols[1],
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[9], xlab = "",
         xaxt = "n",  yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[8:9])
    axis(2, labels = FALSE)
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
#' @param col # color range start/end color
#' @param lineType line type ("l", "b", "o") for delta plot
#' @param legendPos legend position
#' @param ncol number of legend columns 
#' @param ... pars for legend
#'
#' @return NULL
#'
#' @examples
#' \donttest{
#' # Example 1
#' params <- list(amp = seq(20, 30, 2))
#' dmc <- dmcSims(params)
#' plot(dmc, ncol = 2, xlim = c(0, 1300), ylim = c(-100, 200))
#'
#' # Example 2
#' params <- list(amp=c(10, 20), tau = seq(20, 80, 40), drc = seq(0.2, 0.6, 0.2), nTrl = 50000)
#' dmc <- dmcSims(params)
#' plot(dmc, ncol = 2, col=c("green", "blue"), lineType = "l")
#'
#' }
#'
#' @export
plot.dmclist <- function(x,
                         ylim = c(-50, 150),
                         xlim = NULL,
                         col=c("black", "lightgrey"),
                         lineType = "l",
                         legendPos = "topleft",
                         ncol = 1,
                         ...) {
  
  # default xlimit
  if (is.null(xlim)) {
    xlim <- c(0, x[[1]]$prms$tmax)
  }
  
  # colour range
  cols <- colorRampPalette(col)(length(x))
  
  # plot
  plot(x[[1]]$delta$meanBin, x[[1]]$delta$meanEffect, type = lineType,
       ylim = ylim, xlim = xlim,
       ylab = expression(Delta), xlab = "Time [ms]", col = cols[1], ...)

  
  # plot
  plot(NULL, NULL, 
       ylim = ylim, xlim = xlim,
       ylab = expression(Delta), xlab = "Time [ms]", col = cols[1], ...)
    
  legendText <- paste0(names(x[[1]]$params), "=", x[[1]]$params[1, ], collapse = ", ")
  for (i in 1:length(x)) {
    lines(x[[i]]$delta$meanBin, x[[i]]$delta$meanEffect, col = cols[i], type = lineType)
    legendText <- c(NULL, legendText, paste0(names(x[[i]]$params), "=", x[[1]]$params[i, ], collapse = ", "))
  }
  
  legend(legendPos, legend = legendText, col = as.vector(cols), lty = 1, ncol = ncol)
  
}



#' @title plot.dmcob: Plot observed data
#'
#' @description Plot results from the output of dmcObservedData. The plot
#' can be an overall summary, or individual plots (rtCorrect, errorRate,
#' rtErrors, cdf, caf, delta, all). 
#'
#' @param x Output from dmcObservedData
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
#' @param subject NULL (aggregated data across all subjects) or integer for subject number
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
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param ... additional plot pars
#'
#' @return NULL
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
                       resetPar = TRUE,
                       ...) {
  
  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }
  
  figTypes <- c("summary", "all", "rtCorrect", "errorRate", "rtErrors", "cdf", "caf", "delta")
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
  
  showFig = rep(FALSE, 6)
  
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
         ylab = ylabs[1], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)
    if (errorBars) {
      addErrorBars(c(1,2), x$summary$rtCor, x$summary[[paste0(errorBarType, "RtCor")]])
    }
  }
  
  # errorRate
  if (showFig[2]) {
    plot(c(1, 2), x$summary$perErr, type = "o", col = cols[1],
         ylim = ylimEr, xlim = c(0.5, 2.5),
         ylab = ylabs[2], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$perErr, x$summary[[paste0(errorBarType, "PerErr")]])
    }
  }
  
  # rtError
  if (showFig[3]) {
    plot(c(1, 2), x$summary$rtErr, type = "o", col = cols[1],
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[3], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)
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
    if (xaxts == "n") axis(side=1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side=2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    }
    
    lines(x$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "o", col = tail(cols, 2)[2], ...)
    
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
         xaxt = "n", yaxt = "n", ...)
    if (xaxts == "n") {
      axis(side=1, labels = FALSE)  # keep tick marks
    } else if (xaxts == "s" | cafBinLabels) {
      nCAF <- length(x$caf$bin) / 2
      if (cafBinLabels) {
        stepCAF <- 100 / nCAF
        cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
        axis(1, at = seq(1, nCAF, 1), labels = cafLabels, ...)
      } else {
        axis(1, at = seq(1, nCAF, 1), ...)
      }
    } else {
      axis(side = 1,labels = FALSE) 
    }
    
    if (yaxts == "n") {
      axis(side=2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)), ...)
    }
    
    lines(x$caf$accPer[x$caf$Comp == "incomp"],  type = "o", col = tail(cols, 2)[2], ...)
    
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
    axis(side = 1,labels = FALSE) 
    axis(2, labels = FALSE)
    if (errorBars) {
      errorBarCol <- which(grepl(errorBarType, colnames(x$delta)))
      addErrorBars(x$delta$meanBin,
                   x$delta$meanEffect,
                   x$delta[[errorBarCol]],
                   arrowSize = 0.05)
    }
  }
  
}

#' @title plot.dmcobs: Plot combined observed data
#'
#' @description Plot results from the output of dmcObservedData. The plot
#' can be an overall rtCorrect, errorRate, rtErrors, cdf, caf, delta, or all
#' of the previous plots. 
#'
#' @param x Output from dmcObservedData
#' @param figType rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
#' @param subject NULL (aggregated data across all subjects) or integer for subject number
#' @param legend TRUE/FALSE (or FUNCTION) plot legend on each plot
#' @param labels Condition labels c("Compatible", "Incompatible") default
#' @param cols Condition colours c("green", "red") default
#' @param ltys Linetype see par
#' @param pchs Symbols see par 
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
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param ... additional plot pars
#'
#' @return NULL
#'
#' @examples
#' \donttest{
#' # Example 1
#' datFlanker <- dmcObservedData(flankerDataRaw, nDelta = 9)
#' datSimon <- dmcObservedData(simonDataRaw, nDelta = 9)
#' dat <- dmcCombineObservedData(datFlanker, datSimon)  # combine flanker/simon data
#' plot(dat, figType = "delta", xlimDelta = c(200, 700), 
#'      cols = c("black", "darkgrey"), pchs = c(1, 2), resetPar = FALSE)  
#' legend(200, 0, legend = c("Flanker Task", "Simon Task"), 
#'        col = c("black", "darkgrey"), lty = c(1, 1))
#' 
#' }
#' @export
plot.dmcobs <- function(x,
                        figType = "all",
                        subject = NULL,
                        legend = TRUE,
                        labels = c("Compatible", "Incompatible"),
                        cols = c("black", "green", "red"),
                        ltys = c(1, 1),
                        pchs = c(1, 1),
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
                        resetPar = TRUE,
                        ...) {
  
  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }
  
  figTypes <- c("all", "rtCorrect", "errorRate", "rtErrors", "cdf", "caf", "delta")
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
  
  showFig = rep(FALSE, 6)
  
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
  
  # xaxts & yaxts
  xaxts <- ifelse(xaxts, "s", "n")
  yaxts <- ifelse(yaxts, "s", "n")
  
  if (figType == "all") {
    par(mar = c(4, 4, 1, 1), mfrow=c(1, 1), ...)
    showFig[1:6] = TRUE
  } else {
    showFig[figTypes[2:7] %in% figType] = TRUE
  }
  
  # rtCorrect
  if (showFig[1]) {
    
    plot(NULL, NULL,
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[1], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)
    
    for (i in 1:length(x)) {
      lines(c(1, 2), x[[i]]$summary$rtCor, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1,2), x[[i]]$summary$rtCor, x[[i]]$summary[[paste0(errorBarType, "RtCor")]])
      }
    }
    
  }
  
  # errorRate
  if (showFig[2]) {
    
    plot(NULL, NULL, 
         ylim = ylimEr, xlim = c(0.5, 2.5),
         ylab = ylabs[2], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)
   
    for (i in 1:length(x)) { 
      lines(c(1, 2), x[[i]]$summary$perErr, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1, 2), x[[i]]$summary$perErr, x[[i]]$summary[[paste0(errorBarType, "PerErr")]])
      }
    }
    
  }
  
  # rtError
  if (showFig[3]) {
    
    plot(NULL, NULL, 
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = ylabs[3], xlab = "",
         xaxt = "n", yaxt = yaxts, ...)
    axis(1, at = c(1, 2), labels = xlabs[1:2])
    axis(2, labels = FALSE)

    for (i in 1:length(x)) {
      lines(c(1, 2), x[[i]]$summary$rtErr, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i], ...)
      if (errorBars) {
        addErrorBars(c(1, 2), x[[i]]$summary$rtErr, x[[i]]$summary[[paste0(errorBarType, "RtErr")]])
      }
    }
 
  }
  
  # CDF
  if (showFig[4]) {
    
    plot(NULL, NULL, 
         ylim = c(0, 1), xlim = c(200, 1000),
         ylab = ylabs[4], xlab = xlabs[4],
         xaxt = xaxts, yaxt = "n", ...)
    if (xaxts == "n") axis(side=1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side=2, labels = FALSE)  # keep tick marks
    } else if (yaxts ==  "s") {
      axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    }
    
    seqStep <- 100 / (nrow(x[[1]]$delta) + 1)
    for (i in 1:length(x)) { 
      lines(x[[i]]$delta$meanComp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "o",
           ylim = c(0, 1), xlim = c(200, 1000),
           ylab = ylabs[4], xlab = xlabs[4],
           col = tail(cols, 2)[1], lty = ltys[i], pch = pchs[i], 
           xaxt = xaxts, yaxt = "n", ...)
      lines(x[[i]]$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "o", col = tail(cols, 2)[2], ...)
    }
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      if (length(x) == 1) {  # if length > 1, requires custom legend
        legend("bottomright", inset = c(0.025, 0.05), legend = labels, lty = c(1, 1), col = tail(cols, 2), pch = pchs)
      }
    }
    
  }
  
  # caf
  if (showFig[5]) {
    nCAF <- length(x[[1]]$caf$bin) / 2
    plot(NULL, NULL,
         ylim = ylimCAF,
         ylab = ylabs[5], xlab = xlabs[5], xlim = c(0.5, nCAF + 0.5),
         xaxt = "n", yaxt = "n", ...)
    if (xaxts == "n") {
      axis(side=1, labels = FALSE)  # keep tick marks
    } else if (xaxts == "s" | cafBinLabels) {
      if (cafBinLabels) {
        stepCAF <- 100 / nCAF
        cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
        axis(1, at = seq(1, nCAF, 1), labels = cafLabels, ...)
      } else {
        axis(1, at = seq(1, nCAF, 1), ...)
      }
    } else {
      axis(side = 1,labels = FALSE) 
    }
    
    if (yaxts == "n") {
      axis(side=2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)), ...)
    }
    
    for (i in 1:length(x)) { 
      lines(x[[i]]$caf$accPer[x[[i]]$caf$Comp == "comp"],  type = "o", col = tail(cols, 2)[1], lty = ltys[i], pch = pchs[i], ...)
      lines(x[[i]]$caf$accPer[x[[i]]$caf$Comp == "incomp"],  type = "o", col = tail(cols, 2)[2], lty = ltys[i], pch = pchs[i], ...)
    }
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05), legend = labels, lty = c(1, 1), col = tail(cols, 2), pch = pchs)
    }
    
  }
  
  if (showFig[6]) {
    
    plot(NULL, NULL,
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = ylabs[6], xlab = xlabs[6],
         xaxt = xaxts, yaxt = yaxts, ...)
    axis(side = 1,labels = FALSE) 
    axis(2, labels = FALSE)
   
    for (i in 1:length(x)) { 
      lines(x[[i]]$delta$meanBin, x[[i]]$delta$meanEffect, type = "o", col = cols[i], lty = ltys[i], pch = pchs[i])
      if (errorBars) {
        errorBarCol <- which(grepl(errorBarType, colnames(x$delta)))
        addErrorBars(x[[i]]$delta$meanBin,
                     x[[i]]$delta$meanEffect,
                     x[[i]]$delta[[errorBarCol]],
                     arrowSize = 0.05)
      }
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
#' @param subject NULL (aggregated data across all subjects) or integer for subject number
#' @param figType summary, rtCorrect, errorRate, rtErrors, cdf, caf, delta, all
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
#' @param resetPar TRUE/FALSE Reset graphical parameters
#' @param ... additional plot pars
#'
#' @return NULL
#'
#' @examples
#' \donttest{
#' # Example 1
#' resTh <- dmcFit(flankerData, nTrl = 5000)
#' plot(resTh, flankerData)
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
                        resetPar = TRUE,
                        ...) {
  
  # original plot par
  if (resetPar) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }
  
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
  
  if (!is.null(subject)) { 
    # select individual dataset
    subjects <- which(!unlist(lapply(x, is.null)))
    if (!subject %in% subjects) {
      stop("datFit does not contain requested subject number!")
    }
    
    x <- x[[subject]]   
    
    y$summary <- y$summarySubject[y$summarySubject$Subject == subject, ]
    y$delta   <- y$deltaSubject[y$deltaSubject$Subject == subject, ]
    y$caf     <- y$cafSubject[y$cafSubject$Subject == subject, ]
    
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
    par(mar = c(4, 4, 1, 1), mfrow = c(1, 1), ...)
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
    lines(c(x$summary$rtCor), type = "o", lty = 2, ...)
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
    lines(x$summary$perErr, type = "b", lty = 2, ...)
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
    lines(x$summary$rtErr, type = "b", lty = 2, ...)
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
    if (xaxts == "n") axis(side = 1, labels = FALSE)  # keep tick marks
    if (yaxts == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    }
    
    lines(y$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "p", col = tail(cols, 2)[2], ...)
    lines(x$delta$meanComp,   seq(seqStep, 100 - seqStep, seqStep)/100, type = "l", col = tail(cols, 2)[1], ...)
    lines(x$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "l", col = tail(cols, 2)[2], ...)
    
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
    if (xaxts == "n") {
      axis(side = 1, labels = FALSE)  # keep tick marks
    } else if (xaxts == "s") {
      nCAF <- length(x$caf$bin) / 2
      if (cafBinLabels) {
        stepCAF <- 100 / nCAF
        cafLabels <- paste0(paste(seq(0, 100 - stepCAF, stepCAF), seq(stepCAF, 100, stepCAF), sep = "-"), "%")
        axis(1, at = seq(1, nCAF, 1), labels = cafLabels)
      } else {
        axis(1, at = seq(1, nCAF, 1))
      }
    } else {
      axis(sidei = 1,labels = F)      
    }
    
    if (yaxts == "n") {
      axis(side = 2, labels = FALSE)  # keep tick marks
    } else if (yaxts == "s") {
      axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)))
    }
    
    lines(y$caf$accPer[y$caf$Comp == "incomp"], type = "p", col = tail(cols, 2)[2], ...)
    lines(x$caf$accPer[x$caf$Comp == "comp"],   type = "l", col = tail(cols, 2)[1], ...)
    lines(x$caf$accPer[x$caf$Comp == "incomp"], type = "l", col = tail(cols, 2)[2], ...)
    
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
    axis(side = 1, labels = FALSE) 
    axis(side = 2, labels = FALSE) 
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05), legend = labels[3:4], lty = c(0, 1), pch = c(1, NA))
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
#' @return Plot
#'
#' @examples
#' # Example 1
#' plot(c(1, 2), c(450, 500), xlim = c(0.5, 2.5), ylim = c(400, 600), type = "o")
#' addErrorBars(c(1, 2), c(450, 500), errorSize = c(20, 20))
#'
#' # Example 2
#' plot(c(1, 2), c(450, 500), xlim = c(0.5, 2.5), ylim = c(400, 600), type = "o")
#' addErrorBars(c(1, 2), c(450, 500), errorSize = c(20, 40), arrowSize = 0.2)
#'
#' @export
addErrorBars <- function(xpos, ypos, errorSize, arrowSize = 0.1) {
  errBars <- as.data.frame(cbind(xpos, ypos, errorSize))
  with(errBars, arrows(xpos, ypos - errorSize,
                       xpos, ypos + errorSize,
                       arrowSize, 90, 3))
}


