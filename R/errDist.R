#' @title errDist
#'
#' @description Returns a random vector of 0's (correct) and 1's (incorrect) with
#' defined proportions (default = 10\% errors).
#'
#' @param n Number
#' @param proportion Approximate proportion of errors in percentage
#'
#' @return double
#'
#' @examples
#' library(DMCfun)
#'
#' # Example 1
#' x <- errDist(1000, 10)
#' table(x)
#'
#' @export
errDist <- function(n=10000, proportion = 10) {
  return(ifelse(runif(n) <= proportion/100, 1, 0))
}
