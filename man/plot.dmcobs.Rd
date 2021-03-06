% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.dmcobs}
\alias{plot.dmcobs}
\title{plot.dmcobs: Plot combined observed data}
\usage{
\method{plot}{dmcobs}(
  x,
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
  ...
)
}
\arguments{
\item{x}{Output from dmcObservedData}

\item{figType}{rtCorrect, errorRate, rtErrors, cdf, caf, delta, all}

\item{subject}{NULL (aggregated data across all subjects) or integer for subject number}

\item{legend}{TRUE/FALSE (or FUNCTION) plot legend on each plot}

\item{legendLabels}{legend labels}

\item{labels}{Condition labels c("Compatible", "Incompatible") default}

\item{cols}{Condition colours c("green", "red") default}

\item{ltys}{Linetype see par}

\item{pchs}{Symbols see par}

\item{errorBars}{TRUE(default)/FALSE Plot errorbars}

\item{errorBarType}{sd(default), or se}

\item{ylimRt}{ylimit for Rt plots}

\item{ylimErr}{ylimit for error rate plots}

\item{xlimCDF}{xlimit for CDF plot}

\item{ylimCAF}{ylimit for CAF plot}

\item{cafBinLabels}{TRUE/FALSE}

\item{ylimDelta}{ylimit for delta plot}

\item{xlimDelta}{xlimit for delta plot}

\item{xlabs}{TRUE/FALSE}

\item{ylabs}{TRUE/FALSE}

\item{xaxts}{TRUE/FALSE}

\item{yaxts}{TRUE/FALSE}

\item{resetPar}{TRUE/FALSE Reset graphical parameters}

\item{...}{additional plot pars}
}
\description{
Plot delta results from the output of dmcObservedData. The plot
can be an overall rtCorrect, errorRate, rtErrors, cdf, caf, delta, or all
of the previous plots.
}
\examples{
\donttest{
# Example 1
datFlanker <- dmcObservedData(flankerDataRaw, nDelta = 9)
datSimon <- dmcObservedData(simonDataRaw, nDelta = 9)
dat <- dmcCombineObservedData(datFlanker, datSimon)  # combine flanker/simon data
plot(dat, figType = "all",
     cols = c("black", "darkgrey"), pchs = c(1, 2), resetPar = FALSE)
legend(200, 0, legend = c("Flanker Task", "Simon Task"),
       col = c("black", "darkgrey"), lty = c(1, 1))

}
}
