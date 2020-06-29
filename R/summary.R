#' @title summary.dmcsim
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



#' @title summary.dmcfit
#'
#' @description Summary of the simulation results from dmcFitAgg
#'
#' @param object Output from dmcFitAgg
#' @param digits Number of digits in the output
#' @param ... pars
#'
#' @return DataFrame (tibble)
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



#' @title summary.dmcfitvp
#'
#' @description Summary of the simulation results from dmcFitVPs
#'
#' @param object Output from dmcFitVPs
#' @param digits Number of digits in the output
#' @param ... pars
#'
#' @return list of DataFrames (tibbles) with the first being individual participant fitted parameters and the 
#' second being the mean fitted parameters
#'
#' @examples
#' \donttest{
#' # Example 1
#' fitVPs <- dmcFitVPs(flankerData, nTrl = 1000, VP = c(1, 10))
#' summary(fitVPs)
#' fit <- mean(fitVPs)
#' }
#'
#' @export
summary.dmcfitvp <- function(object, digits = 2, ...) {
  
  VPs <- which(!unlist(lapply(object, is.null)))
  outVP <- NULL
  for (VP in VPs) {
    outVP <- rbind(outVP, cbind(VP, tibble::as_tibble(object[[VP]]$par)))
  }
  outVP  <- round(outVP, digits)
  outAvg <- round(as.data.frame(t(colMeans(data.matrix(outVP[, 2:11])))), digits)
  out    <- list(outVP, outAvg)
  
  return(out)
  
}

