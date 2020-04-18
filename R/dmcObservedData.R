#' @title dmcObservedData
#'
#' @description Basic example analysis script to create data object required
#' for observed data. Example raw *.txt files are flankerData.txt and simonData.txt. There 
#' are four critical columns:
#' A column containing participant number
#' A column coding for compatible or incompatible 
#' A column with RT (in ms)
#' A column indicating of the response was correct 
#' @param dat Text file(s) containing the observed data or an R DataFrame (see createDF/addDataDF)
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would result
#' in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#' @param stepDelta Step size for the Delta bins. For example, a step size of 5 would result
#' in 19 CAF bins positioned at 5, 10, 15, ... 85, 90, 95\%.
#' @param outlier Outlier limits in ms (e.g., c(200, 1200))
#' @param columns Name of required columns DEFAULT = c("VP", "Comp", "RT", "Error")
#' @param compCoding Coding for compatibility DEFAULT = c("comp", "incomp")
#' @param errorCoding Coding for errors DEFAULT = c(0, 1))
#' @param quantileType Argument (1-9) from R function quantile specifying the algorithm (?quantile)
#'
#' @return DataFrame
#'
#' @examples
#' # Example 1
#' plot(flankerData)  # flanker data from Ulrich et al. (2015)
#'
#' # Example 2
#' plot(simonData)    # simon data from Ulrich et al. (2015)
#'
#' # Example 3 (Basic behavioural analysis from Ulrich et al. 2015)
#' flankerDat <- tibble::add_column(Task = "flanker", flankerData$summaryVP, .before = 2)
#' simonDat   <- tibble::add_column(Task = "simon",   simonData$summaryVP,   .before = 2)
#' datAgg     <- rbind(rbind(flankerDat, simonDat))
#'
#' datAgg$VP   <- factor(datAgg$VP)
#' datAgg$Task <- factor(datAgg$Task)
#' datAgg$Comp <- factor(datAgg$Comp)
#'
#' aovErr <- aov(perErr ~ Comp*Task + Error(VP/(Comp*Task)), datAgg)
#' summary(aovErr)
#' model.tables(aovErr, type = "mean")
#'
#' aovRt <- aov(rtCor ~ Comp*Task + Error(VP/(Comp*Task)), datAgg)
#' summary(aovRt)
#' model.tables(aovRt, type = "mean")
#'
#' # Example 4
#' dat <- createDF(nVP = 50, nTrl = 100, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"    = c(500, 75, 120),
#'                            "Comp_incomp"  = c(530, 75, 100)),
#'                  Error = list("Comp_comp" = c(3, 2, 2, 1, 1),
#'                             "Comp_incomp" = c(21, 3, 2, 1, 1)))
#' datOb <- dmcObservedData(dat, stepCAF = 20, stepDelta = 5)
#' plot(datOb)
#' plot(datOb, VP = 1)
#'
#' @export
dmcObservedData <- function(dat,
                            stepCAF = 20,
                            stepDelta = 5,
                            outlier = c(200, 1200),
                            columns = c("VP", "Comp", "RT", "Error"),
                            compCoding = c("comp", "incomp"),
                            errorCoding = c(0, 1), 
                            quantileType = 5) {
  
  # single file or multiple files
  if (is.character(dat)) {  
    dat <- readr::read_tsv(dat, col_names = TRUE)
  } 
  
  # select required columns
  dat <- dat %>%
    dplyr::select(tidyselect::all_of(columns))
  if (ncol(dat) < 4) {
    stop("dat does not contain required/requested columns!")
  }
  
  # create default column names
  if (any(names(dat) != c("VP", "Comp", "RT", "Error"))) {
    names(dat) = c("VP", "Comp", "RT", "Error")
  }
  
  # create default column values for comp and error coding
  if (any(compCoding != c("comp", "incomp"))) {
    dat$Comp <- ifelse(dat$Comp == compCoding[1], "comp", "incomp")
  }
  if (any(errorCoding != c(0, 1))) {
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
                     rtCor    = mean(rtCorVP, na.rm = TRUE),
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
    dplyr::filter(RT >= rtMin, RT <= rtMax) %>%
    calculateCAF(.) 
  
  datAgg_caf <- datVP_caf %>%
    dplyr::group_by(Comp, bin) %>%
    dplyr::summarize(accPer = mean(accPerVP))
  
  # DELTA
  datVP_dec <- dat %>%
    dplyr::filter(Error == 0, RT >= rtMin, RT <= rtMax) %>%
    calculateDelta(.)  
  
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
