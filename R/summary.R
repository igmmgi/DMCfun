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
#
#' }
#'
#' @export
summary.dmcsim <- function(object, digits = 1, ...) {
  df <- as.data.frame(object$means) 
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
#' fitAgg <- dmcFitAgg(flankerData, nTrl = 1000)
#' summary(fitAgg)  
#' }
#'
#' @export
summary.dmcfit <- function(object, digits = 2, ...) {
  return(round(do.call(cbind.data.frame, object$par), digits))
}



#' @title summary.dmcfit_subject: dmc fit inddiiviidual summary 
#'
#' @description Summary of the simulation results from dmcFitSubject
#'
#' @param object Output from dmcFitSubject
#' @param digits Number of digits in the output
#' @param ... pars
#'
#' @return list of DataFrames with the first being individual participant fitted parameters and the 
#' second being the mean fitted parameters
#'
#' @examples
#' \donttest{
#' # Example 1
#' fitSubjects <- dmcFitSubject(flankerData, nTrl = 1000, subjects = c(1, 10))
#' summary(fitSubjects)
#' fit <- mean(fitSubjects)
#' }
#'
#' @export
summary.dmcfit_subject <- function(object, digits = 2, ...) {
  
  subjects <- which(!unlist(lapply(object, is.null)))
  outSubject <- NULL
  for (subject in subjects) {
    outSubject <- rbind(outSubject, cbind(subject, as.data.frame(object[[subject]]$par)))
  }
  outSubject <- round(outSubject, digits)
  outAvg     <- round(as.data.frame(t(colMeans(data.matrix(outSubject[, 2:11])))), digits)
  out        <- list(outSubject, outAvg)
  
  return(out)
  
}

