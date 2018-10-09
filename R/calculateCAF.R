#' @title calculateCAF
#'
#' @description Calculate conditional accuracy function (CAF). The data in (dat)
#' should consist of a two column dataframe/matrix with the first column containing
#' the reaction times, and the second column containing a logical vector of
#' errors with 0 being correct, 1 being an error.
#'
#' @param dat Two column dataframe/matrix with the first column containing the
#' reaction times and the second column containing the errors.
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would
#' result in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#'
#' @return vector
#'
#' @examples
#' library(DMCfun)
#'
#' # Example 1
#' rts  <- rtDist(1000)
#' errs <- errDist(1000, 10) # approx 10% errors
#' dat  <- cbind(rts, errs)
#' caf  <- calculateCAF(dat)
#'
#' # Example 2
#' rts  <- rtDist(1000)
#' errs <- errDist(1000, 20) # approx 20% errors
#' dat  <- cbind(rts, errs)
#' caf  <- calculateCAF(dat)
#'
#' @export
calculateCAF <- function(dat, stepCAF = 20){

  if (is.matrix(dat)) {
    dat <- as.data.frame(dat)
  }
  names(dat) <- c("rts", "errs")

  nBins <- 100 / stepCAF

  dat <- dat %>%
    dplyr::mutate(bin = ntile(rts, nBins)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarize(N   = n(),
                     caf = sum(errs == 1)/N)

    return(1 - dat$caf)

}
