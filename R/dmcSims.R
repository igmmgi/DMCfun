#' @title dmcSims
#'
#' @description Run dmcSim with range of input parameters.
#'
#' @param params (list of parameters to dmcSim)
#' @param printInputArgs Print DMC input arguments to console
#' @param printResults Print DMC output to console
#'
#' @return list of dmcsim
#'
#' @examples
#' \dontrun{
#' library(DMCfun)
#'
#' # Example 1
#' params <- list(amp = seq(10, 20, 5), tau = c(50, 100, 150), nTrl = 50000)
#' dmc <- dmcSims(params)
#' plot(dmc[[1]])    # full combination 1
#' plot(dmc)         # delta plots for all combinations
#' plot(dmc[c(1:3)]) # delta plots for specific combinations
#'
#' # Example 2
#' params <- list(amp = seq(10, 20, 5), tau = seq(20, 40, 10), bnds = seq(50, 100, 25))
#' dmc <- dmcSims(params)
#' plot(dmc[[1]])  # combination 1
#' plot(dmc)       # delta plots for all combinations
#' plot(dmc[c(1:3)]) # delta plots for specific combinations
#
#'
#' }
#'
#' @export
dmcSims <- function(params,
                    printInputArgs = FALSE,
                    printResults = FALSE) {

  params  <- expand.grid(params)
  uparams <- params[,lengths(lapply(params, unique)) != 1]
  params <- setNames(split(params, seq(nrow(params))), rownames(params))
  dmc <- vector("list", length(params))

  for (i in 1:length(params)) {

    # inputs for each dmcSim call taken from params + add default of not printing individual results
    dmcInputs <- params[[i]]
    cat("DMC ", i, "of", length(params), ":", paste0(names(dmcInputs), "=", dmcInputs), "\n")
    dmcInputs$printInputArgs <- printInputArgs
    dmcInputs$printResults   <- printResults

    dmc[[i]]        <- do.call(dmcSim, dmcInputs)
    dmc[[i]]$params <- uparams

  }

  class(dmc) <- "dmclist"
  return(dmc)

}



