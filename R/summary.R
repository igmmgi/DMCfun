#' @title summary.dmcsim: dmc simulation summary
#'
#' @description Summary of the overall results from dmcSim
#'
#' @param object Output from dmcSim
#' @param digits Number of digits in the output
#' @param ... pars
#'
#' @return DataFrame
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
#' }
#'
#' # Example 3
#' fit <- dmcFit(flankerData, nTrl=100)
#' summary(fit)
#'
#' # Example 3
#' fit <- dmcFitSubject(flankerData, nTrl=100, subjects=c(1,2,3))
#' summary(fit)
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

  # fit object can be one of four types
  # 1) aggregated participant fit 1 data set
  if ("sim" %in% names(object)) {
    return(round(do.call(cbind.data.frame, object$par), digits))
  }

  # 2) aggregated participant fit 2+ data sets
  if ("sim" %in% names(object[[1]]) && !is.null(object[[1]]$nDataSets) && object[[1]]$nDataSets>=2) {
    out <- NULL
    for (i in 1:length(object)) {
      out <- rbind(out, cbind(DataSet=i, round(do.call(cbind.data.frame, object[[i]]$par), digits)))
    }
    return (out)
  }

  # 3) individual participant fit 1 data set
  if ("sim" %in% names(object[[1]]) && !is.null(object[[1]]$nDataSets) && object[[1]]$nDataSets==1) {
    subjects <- which(!unlist(lapply(object, is.null)))
    outSubject <- NULL
    for (subject in subjects) {
        outSubject <- rbind(outSubject, cbind(subject, as.data.frame(object[[subject]]$par)))
    }
    outSubject <- round(outSubject, digits)
    outAvg     <- round(as.data.frame(t(colMeans(data.matrix(outSubject[, 2:(length(outSubject))])))), digits)

    return(list(outSubject, outAvg))
  }

  # 4) individual participant fits 2+ data sets
  if ("sim" %in% names(object[[1]][[1]]) && object[[1]][[1]]$nDataSets>=2) {

    subjects <- which(!unlist(lapply(object, is.null)))
    outSubject <- NULL
    for (subject in subjects) {
      for (dataset in 1:lengths(object)[subject]) {
        outSubject <- round(rbind(outSubject, cbind(DataSet=dataset, subject=subject,
                                                    do.call(cbind.data.frame, object[[subject]][[dataset]]$par))), digits)
      }
    }
    outAvg <- outSubject %>%
      dplyr::group_by(DataSet) %>%
      dplyr::summarize_all(mean) %>%
      dplyr::select(-subject)

    return (list(outSubject, outAvg))

  }

}




