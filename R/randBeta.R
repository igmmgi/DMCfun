#' @title randBeta
#'
#' @description Draw n random numbers from a beta distribution with shape and
#' scale between limits
#'
#' @param n The number of random numbers to return.
#' @param shape Shape parameter of the underlying beta distribution
#' @param lim The min/max limits to which to scale the distribution
#'
#' @return double
#'
#' @examples
#' # Example 1
#' x <- randBeta()
#'
#' # Example 2
#' x <- randBeta(10000)
#' hist(x, 50)
#'
#' # Example 3
#' x <- randBeta(100000, 3, lim = c(-75, 75))
#' hist(x, 50)
#'
#' # Example 4
#' x <- randBeta(100000, 1, lim = c(-75, 75))  # uniform with shape = 1
#' hist(x, 50)
#'
#' @export
randBeta <- function(n=1, shape=3, lim = c(0, 1)){
  return(rbeta(n, shape, shape) * (lim[2] - lim[1]) + lim[1])
}
