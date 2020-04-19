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
#' }
#'
#' @export
summary.dmcsim <- function(object, ...) {
  return(list(tibble::as_tibble(object$prms[1:9]), object$means))
}