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
#' @export
summary.dmcsim <- function(object, digits = 1, ...) {
  df <- as.data.frame(object$summary)
  df[, c(2:6)] <- round(df[, c(2:6)], digits)
  return(df)
}


#' @title summary.dmcfit: dmc fit aggregate summary
#'
#' @description Summary of the simulation results from dmcFit
#'
#' @param object Output from dmcFit
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
  # aggregated participant fit 1 data set
  return(round(do.call(cbind.data.frame, object$par), digits))
}

#' @title summary.dmcfits: dmc fit aggregate summary (2+ data sets)
#'
#' @description Summary of the simulation results from dmcFit
#'
#' @param object Output from dmcFit
#' @param digits Number of digits in the output
#' @param ... pars
#'
#' @return DataFrame
#'
#' @examples
#' \donttest{
#' # Example 1
#' fitAggs <- dmcFit(list(flankerData, simonData), nTrl = 1000)
#' summary(fitAggs)
#' }
#'
#' @export
summary.dmcfits <- function(object, digits = 2, ...) {
  # aggregated participant fit 2+ data sets
  out <- NULL
  for (i in 1:length(object)) {
    out <- rbind(out, cbind(DataSet = i, round(do.call(cbind.data.frame, object[[i]]$par), digits)))
  }
  return(out)
}


#' @title summary.dmcfit_subject: dmcfit individual subject
#'
#' @description Summary of the simulation results from dmcFitSubjectX
#'
#' @param object Output from dmcFitSubject
#' @param digits Number of digits in the output
#' @param ... pars
#'
#' @return DataFrame
#'
#' @examples
#' \donttest{
#' # Example 1
#' fitSubject <- dmcFitSubject(flankerData, nTrl = 1000, subjects = c(1:3))
#' summary(fitSubject)
#' }
#'
#' @export
summary.dmcfit_subject <- function(object, digits = 2, ...) {
  # individual participant fit 1 data set
  subjects <- which(!unlist(lapply(object, is.null)))
  outSubject <- NULL
  for (subject in subjects) {
    outSubject <- rbind(outSubject, cbind(subject, as.data.frame(object[[subject]]$par)))
  }
  outSubject <- round(outSubject, digits)
  outAvg     <- round(as.data.frame(t(colMeans(data.matrix(outSubject[, 2:(length(outSubject))])))), digits)
  return(list(outSubject, outAvg))
}


#' @title summary.dmcfits_subject: dmc fit aggregate summary
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
#' fitsSubject <- dmcFitSubject(list(flankerData, simonData), nTrl = 1000, subjects = c(1:3))
#' summary(fitsSubject)
#' }
#'
#' @export
summary.dmcfits_subject <- function(object, digits = 2, ...) {

  # individual participant fits 2+ data sets
  subjects <- which(!unlist(lapply(object, is.null)))
  outSubject <- NULL
  for (subject in subjects) {
    for (dataset in 1:lengths(object)[subject]) {
      outSubject <- round(rbind(outSubject, cbind(DataSet = dataset, subject = subject,
                                                  do.call(cbind.data.frame, object[[subject]][[dataset]]$par))), digits)
    }
  }
  outAvg <- outSubject %>%
    dplyr::group_by(DataSet) %>%
    dplyr::summarize_all(mean) %>%
    dplyr::select(-subject)
  return(list(outSubject, outAvg))

}
