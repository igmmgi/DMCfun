#' @title plot.dmcsim
#'
#' @description Plot the simulation results from the output of dmcSim. The plot
#' can be an overall summary, or individual plots (activation, trials, pdf, cdf,
#' caf, delta, all). Plot type summary1 contains an activation plot, example
#' individual trials, the probaility distribution function (PDF), the cumulative
#' distribution function (CDF), the conditional accuracy functin (CAF) and
#' delta plot. This requires that dmcSim is run with fullData = TRUE. Plot type
#' summary2 contains only the PDF, CDF, CAF and delta plots and does not require
#' that dmcSim is run with fullData = TRUE.
#'
#' @param x Output from dmcSim
#' @param figType summary1, summary2, activation, trials, pdf, cdf, caf, delta, all
#' @param newFig TRUE/FALSE
#' @param ... pars
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
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
#' plot(dmc, type = "all")
#' }
#'
#' @export
plot.dmcsim <- function(x,
                        figType = "summary1",
                        newFig = TRUE,
                        ...) {

  if (figType == "summary1" & length(x) != 6) {
    figType = "summary2"
  }

  if (figType %in% c("trials", "activation") & length(x) != 6) {
      stop("plotting trials/activation data requires dmcSim with fullData = TRUE")
  }

  resetFig = FALSE
  if (newFig) {
    par(mfrow = c(1, 1))
  }

  if (figType == "summary1") {
    if (newFig) {
      par(mar = c(4, 4, 2, 2))
      layout(matrix(
                    c(1, 1, 3, 4,
                      1, 1, 3, 4,
                      1, 1, 5, 5,
                      2, 2, 5, 5,
                      2, 2, 6, 6,
                      2, 2, 6, 6),
                    nrow = 6, ncol = 4, byrow = TRUE))
    }

    plot(x, figType = "activation", newFig = FALSE)
    plot(x, figType = "trials",     newFig = FALSE)
    plot(x, figType = "pdf",        newFig = FALSE)
    plot(x, figType = "cdf",        newFig = FALSE)
    plot(x, figType = "caf",        newFig = FALSE)
    plot(x, figType = "delta",      newFig = FALSE)

    resetFig <- TRUE

  } else if (figType == "summary2") {

    if (newFig) {
      par(mar = c(4, 4, 2, 2))
      layout(matrix(
                    c(1, 2,
                      1, 2,
                      3, 3,
                      3, 3,
                      4, 4,
                      4, 4),
                    nrow = 6, ncol = 2, byrow = TRUE))
    }

    plot(x, figType = "pdf",   newFig = FALSE)
    plot(x, figType = "cdf",   newFig = FALSE)
    plot(x, figType = "caf",   newFig = FALSE)
    plot(x, figType = "delta", newFig = FALSE)

    resetFig <- TRUE

  } else if (figType == "activation") {

    plot(c(1:x$prms$tmax), x$sim$eq4, type = "l",
         ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlab = "", ylab = "E[X(t)]")
    lines(c(1:x$prms$tmax), -x$sim$eq4, type = "l", lty = 2)
    lines(c(1:x$prms$tmax), cumsum(rep(mean(x$sim$dr_sp[1]), x$prms$tmax)))
    lines(c(1:x$prms$tmax), x$sim$activation_comp, col = "green")
    lines(c(1:x$prms$tmax), x$sim$activation_incomp, col = "red")
    legend("bottomright", legend = c("Comp", "Incomp"),
           col = c("green", "red"), lty = c(1, 1), inset = c(0.05, 0.05))

  } else if (figType == "trials") {

    plot(c(0, x$prms$tmax), c(x$prms$bnds, x$prms$bnds),
         type = "l", ylim = c(-x$prms$bnds - 20, x$prms$bnds + 20),
         xlab = "Time [ms]", ylab = "X(t)")
    lines(c(0, x$prms$tmax), c(-x$prms$bnds, -x$prms$bnds), type = "l")

    for (trl in c(1:x$prms$nTrlData)) {
      idx <- which(abs(x$trials$trials_comp[[trl]]) >= x$prms$bnds)[1]
      lines(x$trials$trials_comp[[trl]][1:idx], type = "l", col = "green")
      idx <- which(abs(x$trials$trials_incomp[[trl]]) >= x$prms$bnds)[1]
      lines(x$trials$trials_incomp[[trl]][1:idx], type = "l", col = "red")
    }

    legend("bottomright", legend = c("Comp", "Incomp"), col = c("green", "red"),
           lty = c(1, 1), inset = c(0.05, 0.2))

  } else if (figType == "pdf") {

    plot(density(x$sim$rts_comp), col = "green", main = NA,
         ylab = "PDF",  ylim = c(0, 0.01), yaxt = "n",
         xlim = c(0, x$prms$tmax), xlab = "Time [ms]")
    lines(density(x$sim$rts_incomp), col = "red")
    legend("topright", legend = c("Comp", "Incomp"),
           col = c("green", "red"), lty = c(1, 1), inset = c(0.05, 0.05))
    axis(2, at = c(0, 0.005, 0.01), labels = c("0", ".005", ".001") )

  } else if (figType == "cdf") {

    plot(ecdf(x$sim$rts_comp), col = "green", main = NA,
         xlab = "Time [ms]", xlim = c(0, x$prms$tmax),
         ylab = "CDF", yaxt = "n")
    lines(ecdf(x$sim$rts_incomp), col = "red")
    legend("bottomright", legend = c("Comp", "Incomp"), col = c("green", "red"),
           lty = c(1, 1), inset = c(0.05, 0.05))
    axis(2, at = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", ".25", ".5", ".75", "1") )

  } else if (figType == "caf") {

    plot(x$caf$accPer[x$caf$Comp == "comp"],
         type = "b", col = "green",
         xlab = "RT Bin", xaxt = "n",
         ylim = c(0, 1), ylab = "CAF", yaxt = "n")
    lines(x$caf$accPer[x$caf$Comp == "incomp"], type = "b", col = "red")
    legend("bottomright", legend = c("Comp", "Incomp"),
           col = c("green", "red"), lty = c(1, 1), inset = c(0.05, 0.05))
    axis(1, at = seq(1, nrow(x$caf)/2, 1))
    axis(2, at = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", ".25", ".5", ".75", "1") )

  } else if (figType == "delta") {

    plot(x$delta$meanBin, x$delta$meanEffect,
         type = "b",
         xlim = c(0, x$prms$tmax), xlab = "Time [ms]",
         ylab = expression(Delta), yaxt = "n", ylim = c(-50, 150))
    axis(2, at = c(-50, 0, 50, 100, 150), labels = c("-50", "0", "50", "100", "150"))

    } else if (figType == "all" & length(x) == 6) {

    plot(x, figType = "activation", newFig = TRUE)
    plot(x, figType = "trials",     newFig = TRUE)
    plot(x, figType = "pdf",        newFig = TRUE)
    plot(x, figType = "cdf",        newFig = TRUE)
    plot(x, figType = "caf",        newFig = TRUE)
    plot(x, figType = "delta",      newFig = TRUE)

  } else if (figType == "all" & length(x) == 5) {

    plot(x, figType = "pdf",   newFig = TRUE)
    plot(x, figType = "cdf",   newFig = TRUE)
    plot(x, figType = "caf",   newFig = TRUE)
    plot(x, figType = "delta", newFig = TRUE)

  }

  if (resetFig) {
    par(mfrow = c(1, 1))
  }

}
