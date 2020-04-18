#' @title rtDist
#'
#' @description Returns value(s) from a distribution appropriate to simulate reaction times.
#' The distribution is a combined exponential and gaussian distribution called
#' an exponentially modified Gaussian (EMG) distribution or ex-gaussian distribution.
#'
#' @param n Number of observations
#' @param gaussMean Mean of the gaussian distribution
#' @param gaussSD SD of the gaussian distribution
#' @param expRate Rate of the exponential function
#'
#' @return double
#'
#' @examples
#' library(DMCfun)
#'
#' # Example 1
#' x <- rtDist()
#' hist(x, 100)
#'
#' # Example 2
#' x <- rtDist(n=20000, gaussMean=800, gaussSD=50, expRate=100)
#' hist(x, 100)
#'
#' @export
rtDist <- function(n=10000, gaussMean=600, gaussSD=50, expRate=200) {

  expDist <- stats::rexp(n, 1/expRate)
  gaussDist <- stats::rnorm(n, gaussMean, gaussSD)
  return(round(expDist + gaussDist - mean(expDist)))

}
