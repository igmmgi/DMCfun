import re

with open('R/plot.R', 'r') as f:
    code = f.read()

# 1. plot.dmcobs
dmcobs_new = """plot.dmcobs <- function(x,
                        dataset = NULL,
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
                        ...) {
  if (is.null(dataset)) {
    plot(mean(x), figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, ...)
  } else {
    plot(x[[dataset]], figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, ...)
  }
}"""

# 2. plot.dmcfits
dmcfits_new = """plot.dmcfits <- function(x,
                         y,
                         dataset = NULL,
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
  if (is.null(dataset)) {
    plot(mean(x), mean(y), figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, legend = legend, legend.parameters = legend.parameters, ...)
  } else {
    plot(x[[dataset]], y[[dataset]], figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, legend = legend, legend.parameters = legend.parameters, ...)
  }
}"""

# 3. plot.dmcfit_subject
dmcfit_subj_new = """plot.dmcfit_subject <- function(x,
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
    plot(mean(x), y, figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, legend = legend, legend.parameters = legend.parameters, ...)
  } else {
    # Extract data for specific subject
    if (!subject %in% y$summarySubject$Subject) {
      stop("datOb does not contain requested subject number!")
    }
    y_subj <- y
    y_subj$summary <- y$summarySubject[y$summarySubject$Subject == subject, ]
    y_subj$delta   <- y$deltaSubject[y$deltaSubject$Subject == subject, ]
    y_subj$caf     <- y$cafSubject[y$cafSubject$Subject == subject, ]
    
    plot(x[[subject]], y_subj, figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, legend = legend, legend.parameters = legend.parameters, ...)
  }
}"""

# 4. plot.dmcfits_subject
dmcfits_subj_new = """plot.dmcfits_subject <- function(x,
                         y,
                         subject = NULL,
                         dataset = NULL,
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
  if (is.null(subject) && is.null(dataset)) {
    plot(mean(x), mean(y), figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, legend = legend, legend.parameters = legend.parameters, ...)
  } else if (is.null(subject) && !is.null(dataset)) {
    x_dataset <- lapply(x, function(s) s[[dataset]])
    class(x_dataset) <- "dmcfit_subject"
    plot(x_dataset, y[[dataset]], figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, legend = legend, legend.parameters = legend.parameters, ...)
  } else if (!is.null(subject) && is.null(dataset)) {
    plot(x[[subject]], y, figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, legend = legend, legend.parameters = legend.parameters, ...)
  } else {
    plot(x[[subject]][[dataset]], y[[dataset]], subject = subject, figType = figType, labels = labels, cols = cols, ylimRt = ylimRt, ylimErr = ylimErr, xlimCDF = xlimCDF, ylimCAF = ylimCAF, cafBinLabels = cafBinLabels, ylimDelta = ylimDelta, xlimDelta = xlimDelta, xlabs = xlabs, ylabs = ylabs, xaxts = xaxts, yaxts = yaxts, xylabPos = xylabPos, resetPar = resetPar, legend = legend, legend.parameters = legend.parameters, ...)
  }
}"""

# regex replacements
def replace_func(code, func_name, new_code):
    # Matches from the function signature down to its closing bracket
    # This is a bit tricky, let's use a robust pattern since we know the structure.
    # We will find `plot.dmcobs <- function(` and `plot.dmcfits <- function(` etc
    # and replace the block up to the next `#' @title` or `\n\n`
    pattern = re.compile(rf"{func_name} <- function\(.*?\)\s*{{.*?^\}}", re.MULTILINE | re.DOTALL)
    return pattern.sub(new_code, code)

code = replace_func(code, "plot\.dmcobs", dmcobs_new)
code = replace_func(code, "plot\.dmcfits", dmcfits_new)
code = replace_func(code, "plot\.dmcfit_subject", dmcfit_subj_new)
code = replace_func(code, "plot\.dmcfits_subject", dmcfits_subj_new)

with open('R/plot.R', 'w') as f:
    f.write(code)

print("Wrappers successfully rewritten!")
