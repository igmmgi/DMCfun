#' @title summary.dmcsim: dmc simulation summary
#'
#' @description Summary of the overall results from dmcSim
#'
#' @param object Output from dmcSim
#' @param digits Number of digits in the output
#' @param ... pars
#'
#' @return list
#'
#' @examples
#' \donttest{
#' # Example 1
#' dmc <- dmcSim()
#' summary(dmc)
#'
#' # Example 2
#' dmc <- dmcSim(tau = 90)
#' summary(dmc)
#'
#' }
#'
#' @export
summary.dmcsim <- function(object, digits = 1, ...) {
  df <- as.data.frame(object$summary)
  df[, c(2:6)] <- round(df[, c(2:6)], digits)
  return(df)
}



#' @title summary.dmcfit: dmc fit aggregate summary
#'
#' @description Summary of the simulation results from dmcFitAgg
#'
#' @param object Output from dmcFitAgg
#' @param digits Number of digits in the output
#' @param ... pars
#'
#' @return DataFrame
#'
#' @examples
#' \donttest{
#' # Example 1
#' fitAgg <- dmcFit(flankerData, nTrl = 1000)
#' summary(fitAgg)
#' }
#'
#' @export
summary.dmcfit <- function(object, digits = 2, ...) {
  if ("sim" %in% names(object)) {  # aggregated fit
    return(round(do.call(cbind.data.frame, object$par), digits))
  } else {  # individual fits
    subjects <- which(!unlist(lapply(object, is.null)))
    outSubject <- NULL
    for (subject in subjects) {
      outSubject <- rbind(outSubject, cbind(subject, as.data.frame(object[[subject]]$par)))
    }
    outSubject <- round(outSubject, digits)
    outAvg     <- round(as.data.frame(t(colMeans(data.matrix(outSubject[, 2:(length(outSubject))])))), digits)
    out        <- list(outSubject, outAvg)

    return(out)
  }

}
