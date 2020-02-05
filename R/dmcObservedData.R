#' @title dmcObservedData
#'
#' @description Basic example analysis script to create data object required
#' for observed data. Example raw *.txt files are flankerData1.txt, flankerData2.txt,
#' and simonData1.txt.
#'
#' @param dat Text file containing the observed data or R dataframe (see createDF/addDataDF)
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would result
#' in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#' @param stepDelta Step size for the Delta bins. For example, a step size of 5 would result
#' in 19 CAF bins positioned at 5, 10, 15, ... 85, 90, 95\%.
#' @param outlier Outlier limits e.g., c(200, 1200)
#' @param quantileType Argument (1-9) from R function quantile specifying the algorithm (?quantile)
#'
#' @return DataFrame
#'
#' @examples
#' \dontrun{
#' library(psychReport)
#' requiredPackages(c("DMCfun",  "ez", "dplyr"))
#'
#' # Example 1
#' # datOb <- dmcObservedData("flankerData1.txt")
#'
#' # Example 2
#' # datOb <- dmcObservedData("flankerData2.txt")
#'
#' # Example 3
#' # datOb <- dmcObservedData("simonData1.txt")
#'
#' # Example 4
#' # datOb <- dmcObservedData("simonData2.txt")
#'
#' #############################################################################
#' # Example 5 (Basic behavioural analysis from Ulrich et al. 2015)
#' flankerData1$summaryVP <- tibble::add_column(Task = "flanker", flankerData1$summaryVP, .before = 2)
#' simonData1$summaryVP   <- tibble::add_column(Task = "simon", simonData1$summaryVP, .before = 2)
#' datAgg <- rbind(rbind(flankerData1$summaryVP, simonData1$summaryVP))
#'
#' datAgg$VP   <- factor(datAgg$VP)
#' datAgg$Task <- factor(datAgg$Task)
#' datAgg$Comp <- factor(datAgg$Comp)
#'
#' aovErr <- ezANOVA(datAgg, dv = .(perErr), wid = .(VP), within = .(Comp, Task),
#'                   detailed = TRUE, return_aov = TRUE)
#' aovErr <- aovTable(aovErr)
#' aovDispMeans(aovErr)
#'
#' aovRt <- ezANOVA(datAgg, dv = .(rtCor), wid = .(VP), within = .(Comp, Task),
#'                  detailed = TRUE, return_aov = TRUE)
#' aovRt <- aovTable(aovRt)
#' aovDispMeans(aovRt)
#'
#' # Delta
#' flankerData1$deltaVP <- tibble::add_column(Task = "flanker", flankerData1$deltaVP, .before = 2)
#' simonData1$deltaVP   <- tibble::add_column(Task = "simon", simonData1$deltaVP, .before = 2)
#' datDelta             <- rbind(rbind(flankerData1$deltaVP, simonData1$deltaVP))
#'
#' datDelta <- datDelta %>%
#'   dplyr::select(-c("meanBin", "meanEffect")) %>%
#'   tidyr::gather(Comp, rt, -c("VP", "Task", "bin"))
#'
#' datDelta$VP   <- factor(datDelta$VP)
#' datDelta$Task <- factor(datDelta$Task)
#' datDelta$bin  <- factor(datDelta$bin)
#' datDelta$Comp <- factor(datDelta$Comp)
#'
#' aovDelta <- ezANOVA(datDelta, dv = .(rt), wid = .(VP), within = .(Task, Comp, bin),
#'                     detailed = TRUE, return_aov = TRUE)
#' aovDelta <- aovTable(aovDelta)
#'
#' # CAF
#' flankerData1$cafVP <- tibble::add_column(Task = "flanker", flankerData1$cafVP, .before = 2)
#' simonData1$cafVP   <- tibble::add_column(Task = "simon", simonData1$cafVP, .before = 2)
#' datCaf             <- rbind(rbind(flankerData1$cafVP, simonData1$cafVP))
#'
#' datCaf$VP   <- factor(datCaf$VP)
#' datCaf$Task <- factor(datCaf$Task)
#' datCaf$bin  <- factor(datCaf$bin)
#' datCaf$Comp <- factor(datCaf$Comp)
#'
#' aovCaf <- ezANOVA(datCaf, dv = .(accPer), wid = .(VP), within = .(Task, Comp, bin),
#'                   detailed = TRUE, return_aov = TRUE)
#' aovCaf <- aovTable(aovCaf)
#' aovDispMeans(aovCaf)
#'
#' }
#' # Example 6
#' dat <- createDF(nVP = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp"), vals = c(500, 75, 120)),
#'                            list(c("Comp:incomp"), vals = c(530, 75, 100))),
#'                  Error = list(list(c("Comp:comp"), vals = c(3, 2, 2, 1, 1)),
#'                             list(c("Comp:incomp"), vals = c(15, 3, 2, 1, 1))))
#' datOb <- dmcObservedData(dat)
#' plot(datOb)
#' plot(datOb, VP = 1)
#'
#' @export
dmcObservedData <- function(dat,
                            stepCAF = 20,
                            stepDelta = 5,
                            outlier = c(200, 1200),
                            quantileType = 5,
                            columns = c("VP", "Comp", "RT", "Error"),
                            compCoding = c("comp", "incomp"),
                            errorCoding = c(0, 1)) {

  if (is.character(dat)) {
    dat <- readr::read_tsv(dat, col_names = TRUE)
  }

  # select required columns
  dat <- dat %>%
    dplyr::select(all_of(columns))
  if (ncol(dat) < 4) {
    stop("dat does not contain required/requested columns!")
  }

  # create default column names
  if (all(names(dat) != c("VP", "Comp", "RT", "Error"))) {
    names(dat) = c("VP", "Comp", "RT", "Error")
  }

  # create default column values for comp and error coding
  if (all(compCoding != c("comp", "incomp"))) {
    dat$Comp <- ifelse(dat$Comp == compCoding[1], "comp", "incomp")
  }
  if (all(errorCoding != c(0, 1))) {
    dat$Error <- ifelse(dat$Error == errorCoding[1], 0, 1)
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

  return(obj)

}
