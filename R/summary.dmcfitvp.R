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
