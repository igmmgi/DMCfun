#' @title dmcObservedDataRaw
#'
#' @description Basic example analysis script to create data object required
#' for observed data. Example raw *.txt files are included.
#'
#' @param datafile Text file containing the observed data
#' @param stepCAF Number of bins for the conditional accuracy function (CAF)
#' @param stepDelta Number of bins fore the distributional analysis
#' @param outlier Outlier limits e.g., c(200, 1200)
#' @param quantileType 1
#' @param datDir Data directory
#' @param saveData TRUE/FALSE
#'
#' @return DataFrame
#'
#' @examples
#' \dontrun{
#' # Example 1
#' datOb <- dmcObservedDataRaw("flankerData1.txt", saveData = TRUE)
#'
#' # Example 2
#' datOb <- dmcObservedDataRaw("flankerData2.txt", saveData = TRUE)
#'
#' # Example 3
#' datOb <- dmcObservedDataRaw("simonData1.txt", saveData = TRUE)
#'
#' # Example 4
#' datOb <- dmcObservedDataRaw("simonData2.txt", saveData = TRUE)
#'
#'}
#' @export
dmcObservedDataRaw <- function(datafile,
                               stepCAF = 20,
                               stepDelta = 5,
                               outlier = c(200, 1200),
                               quantileType = 5,
                               datDir = NULL,
                               saveData = FALSE) {

  if (is.character(datafile)) {
    dat <- readr::read_tsv(datafile, col_names = TRUE)
  }

  rtMin  <- outlier[1]
  rtMax  <- outlier[2]

  # aggregate data across trials within VPs
  datVP <- dat %>%
    dplyr::mutate(outlier = RT <= rtMin | RT >= rtMax) %>%
    dplyr::group_by(VP, Comp) %>%
    dplyr::summarize(N        = n(),
                     nCorVP   = sum(Error == 0),
                     nErrVP   = sum(Error),
                     nOutVP   = sum(outlier),
                     rtCorVP  = mean(RT[Error == 0 & outlier == 0]),
                     rtErrVP  = mean(RT[Error == 1 & outlier == 0]),
                     perErrVP = (nErrVP/(nErrVP + nCorVP))*100)

  # aggregate data across VPs
  datAgg <- datVP %>%
    dplyr::group_by(Comp) %>%
    dplyr::summarize(N        = n(),
                     NCor     = sum(nCorVP),
                     NErr     = sum(nErrVP),
                     NOut     = sum(nOutVP),
                     rtCor    = mean(rtCorVP,na.rm = TRUE),
                     sdRtCor  = sd(rtCorVP, na.rm = TRUE),
                     seRtCor  = sdRtCor/sqrt(N),
                     rtErr    = mean(rtErrVP, na.rm = TRUE),
                     sdRtErr  = sd(rtErrVP, na.rm = TRUE),
                     seRtErr  = sdRtErr/sqrt(N),
                     perErr   = mean(perErrVP, na.rm = TRUE),
                     sdPerErr = sd(perErrVP, na.rm = TRUE),
                     sePerErr = sdPerErr/sqrt(N))

  # conditional accuracy functions (CAF)
  datVP_caf <- dat %>%
    dplyr::group_by(VP, Comp) %>%
    dplyr::mutate(bin = ntile(RT, length(seq(0, 100 - stepCAF, stepCAF)))) %>%
    dplyr::group_by(VP, Comp, bin) %>%
    dplyr::summarize(N        = n(),
                     accPerVP = sum(Error == 0)/N)  %>%
    dplyr::group_by(VP, Comp, bin) %>%
    dplyr::summarize(accPerVP = mean(accPerVP))

  datAgg_caf <- datVP_caf %>%
    dplyr::group_by(Comp, bin) %>%
    dplyr::summarize(accPer = mean(accPerVP))

  # DELTA
  datVP_dec <- dat %>%
    dplyr::filter(Error == 0, RT >= rtMin, RT <= rtMax) %>%
    dplyr::group_by(VP, Comp) %>%
    dplyr::do(tibble::as_tibble(t(quantile(.$RT, seq(stepDelta, 100 - stepDelta, stepDelta)/100, type = quantileType))))  %>%
    setNames(c("VP", "Comp", seq(1, length(seq(stepDelta, 100 - stepDelta, stepDelta))))) %>%
    tidyr::gather(bin, rt_VP, -c("VP", "Comp")) %>%
    tidyr::spread(Comp, rt_VP) %>%
    dplyr::mutate(meanCompVP   = comp,
                  meanIncompVP = incomp,
                  meanBinVP    = (comp + incomp)/2,
                  meanEffectVP = (incomp - comp)) %>%
    dplyr::select(-dplyr::one_of("comp", "incomp")) %>%
    dplyr::mutate(bin = as.integer(bin)) %>%
    dplyr::arrange(VP, bin)

  datAgg_dec <- datVP_dec %>%
    dplyr::group_by(bin) %>%
    dplyr::summarize(meanComp   = mean(meanCompVP),
                     meanIncomp = mean(meanIncompVP),
                     meanBin    = mean(meanBinVP),
                     meanEffect = mean(meanEffectVP),
                     sdEffect   = sd(meanEffectVP),
                     seEffect   = sdEffect/sqrt(n()))

  ##############################################################################
  # save results
  obj <- list()

  # summary
  obj$summaryVP        <- datVP[ , c(1, 2, 7, 9, 8)]
  obj$summary          <- datAgg[ , c(1, 6, 7, 8, 12, 13, 14, 9, 10, 11)]
  names(obj$summaryVP) <- c("VP", "Comp", "rtCor", "perErr", "rtErr")

  # caf
  obj$cafVP        <- datVP_caf
  names(obj$cafVP) <- c("VP", "Comp", "bin", "accPer")
  obj$caf          <- datAgg_caf

  # delta
  obj$deltaVP        <- datVP_dec
  names(obj$deltaVP) <- c("VP", "bin", "meanComp", "meanIncomp", "meanBin", "meanEffect")
  obj$delta          <- datAgg_dec

  class(obj) <- "dmcob"

  if (saveData) {
    datafile <- tools::file_path_sans_ext(datafile)
    assign(datafile, obj)
    do.call(usethis::use_data, list(as.name(datafile), overwrite = TRUE))
  }

  return(obj)

}
