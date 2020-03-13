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
#' # Example 1 (real dataset)
#' plot(flankerData, cols = c("blue", "red"))
#' plot(flankerData, errorBars = TRUE, errorBarType = "se")
#' plot(flankerData, figType = "delta", errorBars = TRUE,  errorBarType = "se")
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
#'                  RT = list(list(c("Comp:comp"), vals = c(420, 100, 80)),
#'                            list(c("Comp:incomp"), vals = c(470, 100, 95))),
#'                  Error = list(list(c("Comp:comp"), vals = c(5, 3, 2, 1, 2)),
#'                          list(c("Comp:incomp"), vals = c(15, 8, 4, 2, 2))))
#' datOb <- dmcObservedData(dat)
#' plot(datOb, errorBars = TRUE, errorBarType = "sd")
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
                       VP = NULL,
                       legend = TRUE,
                       labels = c("Compatible", "Incompatible"),
                       cols = c("green", "red"),
                       errorBars = FALSE,
                       errorBarType = "sd",
                       ylimRt = c(200, 800),
                       ylimEr = c(0, 20),
                       ylimCAF = c(0, 1),
                       ylimDelta = c(-50, 100),
                       xlimDelta = c(200, 1000),
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
  if (figType == "summary") {
    par(mar = c(4, 4, 2, 2))
    layout(matrix(c(1, 4,
                    2, 5,
                    3, 6),
                  nrow = 3, ncol = 2, byrow = TRUE))
    showFig[1:6] = TRUE
  } else if (figType == "all") {
    par(mar = c(4, 4, 2, 2), mfrow=c(1, 1))
    showFig[1:6] = TRUE
  } else {
    showFig[figTypes[3:8] %in% figType] = TRUE
  }
  
  # rtCorrect
  if (showFig[1]) {
    plot(c(1, 2), x$summary$rtCor, type = "o",
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = "RT Correct [ms]", xlab = "",
         xaxt = "n", ...)
    axis(1, at = c(1, 2), labels = labels)
    if (errorBars) {
      addErrorBars(c(1,2), x$summary$rtCor, x$summary[[paste0(errorBarType, "RtCor")]])
    }
  }
  
  # errorRate
  if (showFig[2]) {
    plot(c(1, 2), x$summary$perErr, type = "o",
         ylim = ylimEr, xlim = c(0.5, 2.5),
         ylab = "Error Rate [%]", xlab = "",
         xaxt = "n", ...)
    axis(1, at = c(1, 2), labels = labels)
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$perErr, x$summary[[paste0(errorBarType, "PerErr")]])
    }
  }
  
  # rtError
  if (showFig[3]) {
    plot(c(1, 2), x$summary$rtErr, type = "o",
         ylim = ylimRt, xlim = c(0.5, 2.5),
         ylab = "RT Error [ms]", xlab = "",
         xaxt = "n", ...)
    axis(1, at = c(1, 2), labels = labels)
    if (errorBars) {
      addErrorBars(c(1, 2), x$summary$rtErr, x$summary[[paste0(errorBarType, "RtErr")]])
    }
  }
  
  # CDF
  if (showFig[4]) {
    seqStep <- 100 / (nrow(x$delta) + 1)
    plot(x$delta$meanComp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "o",
         ylim = c(0, 1), xlim = c(200, 1000),
         ylab = "CDF", xlab = "t [ms]",
         col = cols[1],  yaxt = "n", ...)
    lines(x$delta$meanIncomp, seq(seqStep, 100 - seqStep, seqStep)/100, type = "o", col = cols[2], ...)
    axis(2, at = seq(0, 1, 0.25), labels = as.character(seq(0, 1, 0.25)))
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05), legend = labels, lty = c(1, 1), col = cols, pch = c(1, 1))
    }
    
  }
  
  # caf  
  if (showFig[5]) {
    plot(x$caf$accPer[x$caf$Comp == "comp"],  type = "o",
         ylim = ylimCAF,
         ylab = "CAF", xlab = "RT Bin",
         col = cols[1], yaxt = "n", ...)
    lines(x$caf$accPer[x$caf$Comp == "incomp"],  type = "b", col = cols[2], ...)
    axis(1, at = seq(1, nrow(x$caf)/2, 1))
    axis(2, at = seq(0, 1, 0.2), labels = as.character(seq(0, 1, 0.2)))
    
    if (is.function(legend)) {
      legend()
    } else if (!is.list(legend) && legend == TRUE) {
      legend("bottomright", inset = c(0.025, 0.05), legend = labels, lty = c(1, 1), col = cols, pch = c(1, 1))
    }
    
  }
  
  if (showFig[6]) {
    plot(x$delta$meanBin, x$delta$meanEffect, type = "o",
         ylim = ylimDelta, xlim = xlimDelta,
         ylab = expression("Delta"), xlab = "t [ms]",
         ...)
    if (errorBars) {
      errorBarCol <- which(grepl(errorBarType, colnames(x$delta)))
      addErrorBars(x$delta$meanBin,
                   x$delta$meanEffect,
                   x$delta[[errorBarCol]],
                   arrowSize = 0.05)
    }
  }
  
  # reset par
  par(mfrow=c(1,1))
  
}
