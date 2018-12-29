#' @title calculateCostValue
#'
#' @description Calculate cost value (fit) as combination of RT and error rate
#'
#' @param resTh list containing caf values for comp/incomp conditions (n*2*3) and
#' delta values for comp/incomp conditions (n*2*5). See output from dmcSim.
#' @param resOb list containing caf values for comp/incomp conditions (n*2*3) and
#' delta values for comp/incomp conditions (n*5). See output from dmcSim.
#'
#' @return cost
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1:
#' resTh <- dmcSim()
#' resOb <- dmcSim()
#' cost  <- calculateCostValue(resTh, resOb)
#'
#' # Example 2:
#' resTh <- dmcSim()
#' resOb <- dmcSim(tau = 150)
#' cost  <- calculateCostValue(resTh, resOb)
#'
#' @export
calculateCostValue <- function(resTh, resOb) {

  n_rt  <- nrow(resTh$delta) * 2
  n_err <- nrow(resTh$caf)

  costCAF <- sqrt((1/n_err) * sum((resTh$caf$accPer - resOb$caf$accPer)**2))
  costRT  <- sqrt((1/n_rt)  * sum((resTh$delta[c("meanComp", "meanIncomp")] - resOb$delta[c("meanComp", "meanIncomp")])**2))

  costValue <- (((1 - (n_rt/(n_rt + n_err))) * 1500) * costCAF) + costRT
  cat(sprintf("RMSE: %.3f\n", costValue))

  return(costValue)

}
