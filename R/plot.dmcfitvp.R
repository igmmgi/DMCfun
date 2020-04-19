#' @title plot.dmcfitvp
#'
#' @description Plot the simulation results from the output of dmcFitVPs. The plot
#' can be an overall summary, or individual plots (activation, trials, pdf, cdf,
#' caf, delta, all). Plot type summary1 contains an activation plot, example
#' individual trials, the probability distribution function (PDF), the cumulative
#' distribution function (CDF), the conditional accuracy function (CAF) and
#' delta plots. This required that dmcSim is run with fullData = TRUE. Plot type
#' summary2 contains only the PDF, CDF, CAF and delta plots and does not require
#' that dmcSim is run with fullData = TRUE.
#'
#' @param x Output from dmcFitVPs
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
#' # Example 1
#' resTh <- dmcFitVPs(flankerData, nTrl = 5000, VP = c(2, 3))
#' plot(resTh, flankerData)
#' plot(flankerData, VP = 2)
#' plot(resTh, flankerData)
#' }
#'
#' @export
plot.dmcfitvp <- function(x,
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

  VPs <- which(!unlist(lapply(x, is.null)))
  yvp <- NULL
  for (VP in VPs) {
    
     yvp$summary <- y$summaryVP[y$summaryVP$VP == VP, ]
     yvp$delta   <- y$deltaVP[y$deltaVP$VP == VP, ]
     yvp$caf     <- y$cafVP[y$cafVP$VP == VP, ]
      
     plot(x[[VP]], yvp, 
          figType = "summary", 
          legend = legend,
          labels = labels,
          cols = cols,
          ylimRt = ylimRt,
          ylimEr = ylimEr,
          ylimCAF = ylimCAF,
          cafBinLabels = cafBinLabels,
          ylimDelta = ylimDelta,
          xlimDelta = xlimDelta,
          xlabs = xlabs,
          ylabs = ylabs,
          xaxts = xaxts,
          yaxts = yaxts,
          resetLayout = resetLayout,
          ...)
     
  } 
  
}
  
  