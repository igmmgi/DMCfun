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
