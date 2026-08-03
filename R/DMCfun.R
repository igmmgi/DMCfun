#' @details
#' \strong{Data Structures}
#'
#' The package has 6 distinct classes of data that can be plotted or summarized:
#' \itemize{
#'   \item \strong{dmcob}: A single observed dataset (e.g., Flanker).
#'   \item \strong{dmcobs}: Multiple observed datasets (e.g., Flanker + Simon).
#'   \item \strong{dmcfit}: A single model fit to a single dataset.
#'   \item \strong{dmcfits}: A single model fit across multiple datasets simultaneously.
#'   \item \strong{dmcfit_subject}: A single model fit across multiple subjects (contains individual subject data).
#'   \item \strong{dmcfits_subject}: A single model fit across multiple datasets AND multiple subjects.
#' }
#'
#' @useDynLib DMCfun, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats rt time runif rnorm sd quantile rbeta ecdf density setNames optim
#' @importFrom graphics hist plot lines layout par axis legend abline title
#' @importFrom grDevices dev.off pdf colorRampPalette
#' @importFrom utils data read.table modifyList txtProgressBar setTxtProgressBar tail packageVersion
#' @importFrom methods is
#' @importFrom dplyr n group_by summarize mutate ntile %>%
#' @importFrom tidyr pivot_wider
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pblapply
#'
#' @keywords internal
"_PACKAGE"

#' dmcCppR
#' @name dmcCppR
NULL
