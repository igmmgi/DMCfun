#' @title summary.dmcsim
#'
#' @description Summary of the simulation results from dmcSim
#'
#' @param object Output from dmcSim
#' @param ... pars
#'
#' @return list
#'
#' @examples
#' \dontrun{
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
summary.dmcsim <- function(object, ...) {
  return(list(tibble::as_tibble(object$prms[1:9]), object$means))
}



#' @title summary.dmcfit
#'
#' @description Summary of the simulation results from dmcFitAgg
#'
#' @param object Output from dmcFitAgg
#' @param ... pars
#'
#' @return DataFrame (tibble)
#'
#' @examples
#' \dontrun{
#' # Example 1
#' fitAgg <- dmcFitAgg(flankerData, nTrl = 1000)
#' summary(fitAgg)  # this is just fitAgg$means
#' }
#'
#' @export
summary.dmcfit <- function(object, ...) {
    return(object$means)
}



#' @title summary.dmcfitvp
#'
#' @description Summary of the simulation results from dmcFitVPs
#'
#' @param object Output from dmcFitVPs
#' @param ... pars
#'
#' @return list of DataFrames (tibbles) with the first being individual participant fitted parameters and the 
#' second being the mean fitted parameters
#'
#' @examples
#' \dontrun{
#' # Example 1
#' fitVPs <- dmcFitVPs(flankerData, nTrl = 1000, VP = c(1, 10))
#' summary(fitVPs)
#' fit <- mean(fitVPs)
#' }
#'
#' @export
summary.dmcfitvp <- function(object, ...) {
  
  VPs <- which(!unlist(lapply(object, is.null)))
  outVP <- NULL
  for (VP in VPs) {
    outVP <- rbind(outVP, cbind(VP, tibble::as_tibble(object[[VP]]$par)))
  }
  outAvg <- tibble::as_tibble(t(c(colMeans(outVP[, c(2:11)]))))
  out    <- list(outVP, outAvg)
  
  return(out)
  
}

