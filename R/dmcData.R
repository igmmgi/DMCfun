#' A summarised dataset: This is the flanker task data from Ulrich et al. (2015)
#'
#' \itemize{
#'   \item $summary --> Reaction time correct, standard deviation correct, standard error correct,
#'   percentage error, standard deviation error, standard error error, reaction time incorrect,
#'   standard deviation incorrect, and standard error incorrect trials for both compatible
#'   and incompatible trials
#'   \item $caf --> Proportion correct for compatible and incompatible trials across 5 bins
#'   \item $delta --> Compatible reactions times, incompatible mean reaction times,
#'   mean reaction times, incompatible - compatible reaction times (effect), and
#'   standard deviation + standard error of this effect across 19 bins
#'   \item $data --> Raw data from flankerData.txt + additional outlier column
#' }
#'
#' @docType data
#' @keywords datasets
#' @name flankerData
#' @usage flankerData
#' @format dmcob
NULL


#' A summarised dataset: This is the simon task data from Ulrich et al. (2015)
#'
#' \itemize{
#'   \item $summary --> Reaction time correct, standard deviation correct, standard error correct,
#'   percentage error, standard deviation error, standard error error, reaction time incorrect,
#'   standard deviation incorrect, and standard error incorrect trials for both compatible
#'   and incompatible trials
#'   \item $caf --> Proportion correct for compatible and incompatible trials across
#'   5 bins
#'   \item $delta --> Compatible reactions times, incompatible mean reaction times,
#'   mean reaction times, incompatible - compatible reaction times (effect), and
#'   standard deviation + standard error of this effect across 19 bins
#'   \item $data --> Raw data from simonData.txt + additional outlier column
#' }
#'
#' @docType data
#' @keywords datasets
#' @name simonData
#' @usage simonData
#' @format dmcob
NULL


#' @title createDF
#'
#' @description Create dataframe (see also addDataDF)
#'
#' @param nSubjects Number of subjects
#' @param nTrl Number of trials per factor/level for each participant
#' @param design Factors and levels
#'
#' @return DataFrame with Subject, Factor(s) columns
#'
#' @examples
#' # Example 1
#' dat <- createDF()
#'
#' # Example 2
#' dat <- createDF(nSubjects = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#'
#' # Example 3
#' dat <- createDF(nSubjects = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp"),
#'                                                          "Side" = c("left", "right", "middle")))
#'
#' @export
createDF <- function(nSubjects = 20,
                     nTrl = 50,
                     design = list("A" = c("A1", "A2"), "B" = c("B1", "B2"))) {

  dat <-  data.frame(expand.grid(modifyList(design, list(Subject = c(1:nSubjects), Trial = c(1:nTrl)))))
  return(dat[c("Subject", names(design))])

}



#' @title addDataDF
#'
#' @description Add simulated ex-gaussian reaction-time (RT) data and
#' binary error (Error = 1, Correct = 0) data to an R DataFrame. This function
#' can be used to create simulated data sets.
#'
#' @param dat DataFrame (see createDF)
#' @param RT RT parameters (see rtDist)
#' @param Error Error parameters (see errDist)
#'
#' @return DataFrame with RT (ms) and Error (bool) columns
#'
#' @examples
#' # Example 1: default dataframe
#' dat <- createDF()
#' dat <- addDataDF(dat)
#' head(dat)
#' hist(dat$RT, 100)
#' table(dat$Error)
#'
#' # Example 2: defined overall RT parameters
#' dat <- createDF(nSubjects = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat, RT = c(500, 150, 100))
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#'
#' # Example 3: defined RT + Error parameters across conditions
#' dat <- createDF(nSubjects = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(500, 80, 100),
#'                            "Comp_incomp" = c(600, 80, 140)),
#'                  Error = list("Comp_comp"   = 5,
#'                               "Comp_incomp" = 15))
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#'
#' # Example 4:
#' # create dataframe with defined RT + Error parameters across different conditions
#' dat <- createDF(nSubjects = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"      = c(500, 150, 100),
#'                            "Comp_neutral"   = c(550, 150, 100),
#'                            "Comp_incomp"    = c(600, 150, 100)),
#'                  Error = list("Comp_comp"    =  5,
#'                               "Comp_neutral" = 10,
#'                               "Comp_incomp"  = 15))
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#'
#' # Example 5:
#' # create dataframe with defined RT + Error parameters across different conditions
#' dat <- createDF(nSubjects = 50, nTrl = 50,
#'                 design = list("Hand" = c("left", "right"),
#'                               "Side" = c("left", "right")))
#' dat <- addDataDF(dat,
#'                  RT = list("Hand:Side_left:left"   = c(400, 150, 100),
#'                            "Hand:Side_left:right"  = c(500, 150, 100),
#'                            "Hand:Side_right:left"  = c(500, 150, 100),
#'                            "Hand:Side_right:right" = c(400, 150, 100)),
#'                  Error = list("Hand:Side_left:left"   = c(5,4,2,2,1),
#'                               "Hand:Side_left:right"  = c(15,4,2,2,1),
#'                               "Hand:Side_right:left"  = c(15,7,4,2,1),
#'                               "Hand:Side_right:right" = c(5,8,5,3,1)))
#'
#' boxplot(dat$RT ~ dat$Hand + dat$Side)
#' table(dat$Error, dat$Hand, dat$Side)
#'
#' @export
addDataDF <- function(dat, RT=NULL, Error=NULL) {

  # reaction time
  dat$RT <- 0
  if (is.null(RT)) {
    dat$RT <- rtDist(n = nrow(dat))
  } else if (!is.null(RT) & is.double(RT)) {
    dat$RT <- rtDist(n = nrow(dat), RT[1], RT[2], RT[3])
  } else if (!is.null(RT) & is.list(RT)) {

    for (i in seq_along(RT)) {

      fcts_levls <- unlist(strsplit(names(RT[i]),  split = "_"))
      fcts       <- unlist(strsplit(fcts_levls[1], split = ":"))
      levls      <- unlist(strsplit(fcts_levls[2], split = ":"))

      idx <- NULL
      for (fct in seq_along(fcts)) {
        idx <- cbind(idx, dat[fcts[fct]] == levls[fct])
      }
      idx         <- apply(idx, 1, all)
      dat$RT[idx] <- rtDist(n = sum(idx), RT[[i]][1], RT[[i]][2], RT[[i]][3])

    }

  }

  # error rate
  dat$Error <- 0
  dat$bins  <- 0
  if (is.null(Error)) {
    dat$Error <- errDist(n = nrow(dat))
  } else if (!is.null(Error) & is.double(Error)) {
    dat$Error <- errDist(n = nrow(dat), Error)
  } else if (!is.null(Error) & is.list(Error)) {

    for (i in seq_along(Error)) {

      fcts_levls <- unlist(strsplit(names(Error[i]), split = "_"))
      fcts       <- unlist(strsplit(fcts_levls[1],   split = ":"))
      levls      <- unlist(strsplit(fcts_levls[2],   split = ":"))

      idx <- NULL
      for (fct in seq_along(fcts)) {
        idx <- cbind(idx, dat[fcts[fct]] == levls[fct])
      }
      idx <- apply(idx, 1, all)

      dat$bins[idx] <- dplyr::ntile(dat$RT[idx], length(Error[[1]]))
      for (bin in seq_along(Error[[1]])) {
        idx_bin <- dat$bins == bin & idx
        dat$Error[idx_bin] <- errDist(n = sum(idx_bin), Error[[i]][bin])
      }

    }
  }

  if ("bins" %in% names(dat)) {
    dat <- dat[, -which(names(dat) %in% c("bins"))]
  }

  return(dat)

}



#' @title dmcObservedData
#'
#' @description Basic analysis to create data object required for observed data.
#' Example raw *.txt files are flankerData.txt and simonData.txt. There are four critical columns:
#' \enumerate{
#' \item column containing subject number
#' \item column coding for compatible or incompatible
#' \item column with RT (in ms)
#' \item column indicating of the response was correct
#' }
#' @param dat A text file(s) containing the observed data or an R DataFrame (see createDF/addDataDF)
#' @param nCAF The number of CAF bins.
#' @param nDelta The number of delta bins.
#' @param pDelta An alternative option to nDelta by directly specifying required percentile values (vector of values 0-100)
#' @param tDelta The type of delta calculation (1=direct percentiles points, 2=percentile bounds (tile) averaging)
#' @param outlier Outlier limits in ms (e.g., c(200, 1200))
#' @param columns Name of required columns DEFAULT = c("Subject", "Comp", "RT", "Error")
#' @param compCoding Coding for compatibility DEFAULT = c("comp", "incomp")
#' @param errorCoding Coding for errors DEFAULT = c(0, 1))
#' @param quantileType Argument (1-9) from R function quantile specifying the algorithm (?quantile)
#' @param keepRaw TRUE/FALSE
#' @param delim Single character used to separate fields within a record if reading from external text file.
#' @param skip The number of lines to skip before reading data if reading from external text file.
#'
#' @return dmcObservedData returns an object of class "dmcob" with the following components:
#' \item{summarySubject}{DataFrame within individual subject data (rtCor, perErr, rtErr) for compatibility condition}
#' \item{summary}{DataFrame within aggregated subject data (rtCor, sdRtCor, seRtCor, perErr, sdPerErr, sePerErr, rtErr, sdRtErr, seRtErr) for compatibility condition}
#' \item{cafSubject}{DataFrame within individual subject conditional accuracy function (CAF) data (Bin, accPerComp, accPerIncomp, meanEffect)}
#' \item{caf}{DataFrame within aggregated subject conditional accuracy function (CAF) data (Bin, accPerComp, accPerIncomp, meanEffect, sdEffect, seEffect)}
#' \item{deltaSubject}{DataFrame within individual subject distributional delta analysis data correct trials (Bin, meanComp, meanIncomp, meanBin, meanEffect)}
#' \item{delta}{DataFrame within aggregated subject distributional delta analysis data correct trials (Bin, meanComp, meanIncomp, meanBin, meanEffect, sdEffect, seEffect)}
#' \item{deltaErrorsSubject}{DataFrame within individual subject distributional delta analysis data incorrect trials (Bin, meanComp, meanIncomp, meanBin, meanEffect)}
#' \item{deltaErrors}{DataFrame within aggregated subject distributional delta analysis data incorrect trials (Bin, meanComp, meanIncomp, meanBin, meanEffect, sdEffect, seEffect)}
#'
#' @examples
#' # Example 1
#' plot(flankerData)  # flanker data from Ulrich et al. (2015)
#' plot(simonData)    # simon data from Ulrich et al. (2015)
#'
#' # Example 2 (Basic behavioural analysis from Ulrich et al. 2015)
#' flankerDat <- cbind(Task = "flanker", flankerData$summarySubject)
#' simonDat   <- cbind(Task = "simon",   simonData$summarySubject)
#' datAgg     <- rbind(flankerDat, simonDat)
#'
#' datAgg$Subject <- factor(datAgg$Subject)
#' datAgg$Task    <- factor(datAgg$Task)
#' datAgg$Comp    <- factor(datAgg$Comp)
#'
#' aovErr <- aov(perErr ~ Comp*Task + Error(Subject/(Comp*Task)), datAgg)
#' summary(aovErr)
#' model.tables(aovErr, type = "mean")
#'
#' aovRt <- aov(rtCor ~ Comp*Task + Error(Subject/(Comp*Task)), datAgg)
#' summary(aovRt)
#' model.tables(aovRt, type = "mean")
#'
#' # Example 3
#' dat <- createDF(nSubjects = 50, nTrl = 500, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"    = c(500, 75, 120),
#'                            "Comp_incomp"  = c(530, 75, 100)),
#'                  Error = list("Comp_comp" = c(3, 2, 2, 1, 1),
#'                             "Comp_incomp" = c(21, 3, 2, 1, 1)))
#' datOb <- dmcObservedData(dat)
#' plot(datOb)
#' plot(datOb, subject = 1)
#'
#' # Example 4
#' dat <- createDF(nSubjects = 50, nTrl = 500, design = list("Congruency" = c("cong", "incong")))
#' dat <- addDataDF(dat,
#'                  RT = list("Congruency_cong"   = c(500, 75, 100),
#'                            "Congruency_incong" = c(530, 100, 110)),
#'                  Error = list("Congruency_cong"   = c(3, 2, 2, 1, 1),
#'                               "Congruency_incong" = c(21, 3, 2, 1, 1)))
#' datOb <- dmcObservedData(dat, nCAF = 5, nDelta = 9,
#'                          columns = c("Subject", "Congruency", "RT", "Error"),
#'                          compCoding = c("cong", "incong"))
#' plot(datOb, labels = c("Congruent", "Incongruent"))
#' plot(datOb, subject = 1)
#'
#' @export
dmcObservedData <- function(dat,
                            nCAF         = 5,
                            nDelta       = 19,
                            pDelta       = vector(),
                            tDelta       = 1,
                            outlier      = c(200, 1200),
                            columns      = c("Subject", "Comp", "RT", "Error"),
                            compCoding   = c("comp", "incomp"),
                            errorCoding  = c(0, 1),
                            quantileType = 5,
                            keepRaw      = FALSE,
                            delim        = "\t",
                            skip         = 0) {

  # if dat external file(s)
  if (is.character(dat)) {
    dat <- do.call(rbind, lapply(dat, read.table, header = TRUE, sep = delim, skip = skip))
  }

  # select required columns and give default names
  dat <- dat[columns]
  if (ncol(dat) < 4) {
    stop("dat does not contain required/requested columns!")
  }
  if (any(names(dat) != c("Subject", "Comp", "RT", "Error"))) {
    names(dat) <- c("Subject", "Comp", "RT", "Error")
  }

  # create default column values for comp and error coding
  if (any(compCoding != c("comp", "incomp"))) {
    dat$Comp <- ifelse(dat$Comp == compCoding[1], "comp", "incomp")
  }
  if (any(errorCoding != c(0, 1))) {
    dat$Error <- ifelse(dat$Error == errorCoding[1], 0, 1)
  }

  # add column for outliers
  dat$outlier <- ifelse(dat$RT <= outlier[1] | dat$RT >= outlier[2], 1, 0)

  # aggregate data across trials within subjects
  datSubject <- dat %>%
    dplyr::group_by(Subject, Comp) %>%
    dplyr::summarize(N       = n(),
                     nCor    = sum(Error == 0),
                     nErr    = sum(Error),
                     nOut    = sum(outlier),
                     rtCor   = mean(RT[Error == 0 & outlier == 0]),
                     rtErr   = mean(RT[Error == 1 & outlier == 0]),
                     perErr  = (nErr / (nErr + nCor)) * 100,
                     .groups = "drop")

  # aggregate data across subjects
  datAgg <- datSubject %>%
    dplyr::mutate(rtC = rtCor,
                  rtE = rtErr,
                  perE = perErr) %>%
    dplyr::group_by(Comp) %>%
    dplyr::summarize(N        = n(),
                     rtCor    = mean(rtC, na.rm = TRUE),
                     sdRtCor  = sd(rtC, na.rm = TRUE),
                     seRtCor  = sdRtCor / sqrt(N),
                     rtErr    = mean(rtE, na.rm = TRUE),
                     sdRtErr  = sd(rtE, na.rm = TRUE),
                     seRtErr  = sdRtErr / sqrt(N),
                     perErr   = mean(perE, na.rm = TRUE),
                     sdPerErr = sd(perE, na.rm = TRUE),
                     sePerErr = sdPerErr / sqrt(N),
                     .groups  = "drop")

  # conditional accuracy functions (CAF)
  datSubject_caf <- dat %>%
    dplyr::filter(outlier == 0) %>%
    calculateCAF(., nCAF = nCAF)

  datAgg_caf <- datSubject_caf %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarize(accPerComp   = mean(comp),
                     accPerIncomp = mean(incomp),
                     meanEffect   = mean(effect),
                     sdEffect     = sd(effect),
                     seEffect     = sdEffect/sqrt(n()),
                     .groups      = "drop")

  # change nDelta to length of pDelta if pDelta not empty
  if (length(pDelta) != 0) {
    nDelta <- length(pDelta)
  }

  # DELTA
  datSubject_dec <- dat %>%
    dplyr::filter(Error == 0, outlier == 0) %>%
    calculateDelta(., nDelta = nDelta, tDelta = tDelta, quantileType = quantileType)

  datSubject_dec_errors <- dat %>%
    dplyr::filter(Error == 1, outlier == 0) %>%
    calculateDelta(., nDelta = nDelta, tDelta = tDelta, quantileType = quantileType)

  datAgg_dec <- datSubject_dec %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarize(meanComp   = mean(comp),
                     meanIncomp = mean(incomp),
                     meanBin    = mean(meanBin),
                     meanEffect = mean(Effect),
                     sdEffect   = sd(Effect),
                     seEffect   = sdEffect / sqrt(n()),
                     .groups    = "drop")

  datAgg_dec_errors <- datSubject_dec_errors %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarize(meanComp   = mean(comp, na.rm = TRUE),
                     meanIncomp = mean(incomp, na.rm = TRUE),
                     meanBin    = mean(Bin, na.rm = TRUE),
                     meanEffect = mean(Effect, na.rm = TRUE),
                     sdEffect   = sd(Effect, na.rm = TRUE),
                     seEffect   = sdEffect / sqrt(n()),
                     .groups    = "drop")

  ##############################################################################
  # save results
  obj <- list()

  if (keepRaw) {
    obj$data <- dat
  }

  # summary
  obj$summarySubject <- as.data.frame(datSubject[, c(1, 2, 7, 9, 8)])
  obj$summary        <- as.data.frame(datAgg[, c(1, 3, 4, 5, 9, 10, 11, 6, 7, 8)])

  # caf
  obj$cafSubject        <- as.data.frame(datSubject_caf)
  names(obj$cafSubject) <- c("Subject", "Bin", "accPerComp", "accPerIncomp", "meanEffect")
  obj$caf               <- as.data.frame(datAgg_caf)

  # delta
  obj$deltaSubject        <- as.data.frame(datSubject_dec)
  names(obj$deltaSubject) <- c("Subject", "Bin", "meanComp", "meanIncomp", "meanBin", "meanEffect")
  obj$delta               <- as.data.frame(datAgg_dec)

  obj$deltaErrorsSubject        <- as.data.frame(datSubject_dec_errors)
  names(obj$deltaErrorsSubject) <- c("Subject", "Bin", "meanComp", "meanIncomp", "meanBin", "meanEffect")
  obj$deltaErrors               <- as.data.frame(datAgg_dec_errors)

  class(obj) <- "dmcob"

  return(obj)

}

#' @title dmcCombineObservedData
#'
#' @description Combine observed datasets
#' @param ... Any number of outputs from dmcObservedData
#'
#' @return dmcCombineObservedData returns a list of objects of class "dmcob"
#'
#' @examples
#' # Example 1
#' dat <- dmcCombineObservedData(flankerData, simonData)  # combine flanker/simon data
#' plot(dat, figType = "delta", xlimDelta = c(200, 700), ylimDelta = c(-20, 80),
#'      cols = c("black", "darkgrey"), legend = FALSE, resetPar = FALSE)
#' legend(200, 80, legend = c("Flanker Task", "Simon Task"),
#'        col = c("black", "darkgrey"), lty = c(1, 1))
#'
#' @export
dmcCombineObservedData <- function(...) {
  dat <- list(...)
  class(dat) <- "dmcobs"
  return(dat)
}


#' @title calculateCAF
#'
#' @description Calculate conditional accuracy function (CAF).
#' The DataFrame should contain columns defining the participant, compatibility condition,
#' RT and error (Default column names: "Subject", "Comp", "RT", "Error"). The "Comp" column should
#' define compatibility condition (Default: c("comp", "incomp")) and the "Error" column should
#' define if the trial was an error or not (Default: c(0, 1) ).
#'
#' @param dat DataFrame with columns containing the participant number, condition
#' compatibility, RT data (in ms) and an Error column.
#' @param nCAF Number of CAF bins.
#' @param columns Name of required columns Default: c("Subject", "Comp", "RT", "Error")
#' @param compCoding Coding for compatibility Default: c("comp", "incomp")
#' @param errorCoding Coding for errors Default: c(0, 1))
#'
#' @return calculateCAF returns a DataFrame with conditional accuracy function (CAF) data (Bin, comp, incomp, effect)
#'
#' @examples
#' # Example 1
#' dat <- createDF(nSubjects = 1, nTrl = 10000, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(500, 80, 100),
#'                            "Comp_incomp" = c(600, 80, 140)),
#'                  Error = list("Comp_comp"   = c( 5, 4, 3, 2, 1),
#'                               "Comp_incomp" = c(20, 8, 6, 4, 2)))
#' caf <- calculateCAF(dat)
#'
#' # Example 2
#' dat <- createDF(nSubjects = 1, nTrl = 10000, design = list("Congruency" = c("cong", "incong")))
#' dat <- addDataDF(dat,
#'                  RT = list("Congruency_cong"   = c(500, 80, 100),
#'                            "Congruency_incong" = c(600, 80, 140)),
#'                  Error = list("Congruency_cong"   = c( 5, 4, 3, 2, 1),
#'                               "Congruency_incong" = c(20, 8, 6, 4, 2)))
#' head(dat)
#' caf <- calculateCAF(dat, columns = c("Subject", "Congruency", "RT", "Error"),
#'                     compCoding = c("cong", "incong"))
#'
#'
#' @export
calculateCAF <- function(dat,
                         nCAF = 5,
                         columns = c("Subject", "Comp", "RT", "Error"),
                         compCoding = c("comp", "incomp"),
                         errorCoding = c(0, 1)) {

  # select required columns
  dat <- dat[columns]
  if (ncol(dat) < 4) {
    stop("dat does not contain required/requested columns!")
  }

  # create default column names
  if (any(names(dat) != c("Subject", "Comp", "RT", "Error"))) {
    names(dat) <- c("Subject", "Comp", "RT", "Error")
  }

  # create default column values for comp and error coding
  if (any(compCoding != c("comp", "incomp"))) {
    dat$Comp <- ifelse(dat$Comp == compCoding[1], "comp", "incomp")
  }
  if (any(errorCoding != c(0, 1))) {
    dat$Error <- ifelse(dat$Error == errorCoding[1], 0, 1)
  }

  # conditional accuracy functions (CAF)
  dat_caf <- dat %>%
    dplyr::group_by(Subject, Comp) %>%
    dplyr::mutate(Bin = ntile(RT, nCAF)) %>%
    dplyr::group_by(Subject, Comp, Bin) %>%
    dplyr::summarize(accPer  = sum(Error == 0) / n(),
                     .groups = "drop")  %>%
    dplyr::group_by(Subject, Comp, Bin) %>%
    dplyr::summarize(accPer  = mean(accPer),
                     .groups = "drop") %>%
    tidyr::pivot_wider(., id_cols = c("Subject", "Bin"), names_from = "Comp", values_from = "accPer") %>%
    dplyr::mutate(effect = ((100 - incomp) - (100 - comp)) * 100)

  return(dat_caf)

}


#' @title calculateDelta
#'
#' @description Calculate delta plot. Here RTs are split into n bins (Default: 5) for compatible and
#' incompatible trials separately. Mean RT is calculated for each condition in each bin then
#' subtracted (incompatible - compatible) to give a compatibility effect (delta) at each bin.
#'
#' @param dat DataFrame with columns containing the participant number, condition
#' compatibility, and RT data (in ms).
#' @param nDelta The number of delta bins.
#' @param tDelta type of delta calculation (1=direct percentiles points, 2=percentile bounds (tile) averaging)
#' @param columns Name of required columns Default: c("Subject", "Comp", "RT")
#' @param compCoding Coding for compatibility Default: c("comp", "incomp")
#' @param quantileType Argument (1-9) from R function quantile specifying the algorithm (?quantile)
#'
#' @return calculateDelta returns a DataFrame with distributional delta analysis data (Bin, comp, incomp, meanBin, Effect)
#'
#' @examples
#' # Example 1
#' dat <- createDF(nSubjects = 1, nTrl = 10000, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(500, 80, 100),
#'                            "Comp_incomp" = c(600, 80, 140)))
#' delta <- calculateDelta(dat)
#'
#' # Example 2
#' dat <- createDF(nSubject = 1, nTrl = 10000, design = list("Congruency" = c("cong", "incong")))
#' dat <- addDataDF(dat,
#'                  RT = list("Congruency_cong"   = c(500, 80, 100),
#'                            "Congruency_incong" = c(600, 80, 140)))
#' head(dat)
#' delta <- calculateDelta(dat, nDelta = 9, columns = c("Subject", "Congruency", "RT"),
#'                         compCoding = c("cong", "incong"))
#'
#'
#' @export
calculateDelta <- function(dat,
                           nDelta = 19,
                           tDelta = 1,
                           columns = c("Subject", "Comp", "RT"),
                           compCoding = c("comp", "incomp"),
                           quantileType = 5) {

  # select required columns
  dat <- dat[columns]
  if (ncol(dat) != 3) {
    stop("dat does not contain required/requested columns!")
  }

  # create default column names
  if (any(names(dat) != c("Subject", "Comp", "RT"))) {
    names(dat) <- c("Subject", "Comp", "RT")
  }

  # create default column values for comp and error coding
  if (any(compCoding != c("comp", "incomp"))) {
    dat$Comp <- ifelse(dat$Comp == compCoding[1], "comp", "incomp")
  }


  if (tDelta == 1) {

    deltaSeq <- seq(0, 100, length.out = nDelta + 2)
    deltaSeq <- deltaSeq[2:(length(deltaSeq) - 1)]

    dat_delta <- dat %>%
      dplyr::group_by(Subject, Comp) %>%
      dplyr::summarize(Bin    = seq(1, length(deltaSeq)),
                       rt      = quantile(RT, deltaSeq / 100, type = quantileType),
                       .groups = "drop")  %>%
      tidyr::pivot_wider(., id_cols = c("Subject", "Bin"), names_from = "Comp", values_from = "rt") %>%
      dplyr::mutate(meanBin = (comp + incomp) / 2,
                    Effect  = (incomp - comp))

  } else if (tDelta == 2) {

    dat_delta <- dat %>%
      dplyr::group_by(Subject, Comp) %>%
      dplyr::mutate(Bin = ntile(RT, nDelta + 1)) %>%
      dplyr::group_by(Subject, Comp, Bin) %>%
      dplyr::summarize(rt = mean(RT),
                       .groups = "drop")  %>%
      tidyr::pivot_wider(., id_cols = c("Subject", "Bin"), names_from = "Comp", values_from = "rt") %>%
      dplyr::mutate(meanBin = (comp + incomp) / 2,
                    Effect  = (incomp - comp))

  }

  return(dat_delta)

}



#' @title rtDist
#'
#' @description Returns value(s) from a distribution appropriate to simulate reaction times.
#' The distribution is a combined exponential and gaussian distribution called
#' an exponentially modified Gaussian (EMG) distribution or ex-gaussian distribution.
#'
#' @param n Number of observations
#' @param gaussMean Mean of the gaussian distribution
#' @param gaussSD SD of the gaussian distribution
#' @param expRate Rate of the exponential function
#'
#' @return double
#'
#' @examples
#' # Example 1
#' x <- rtDist()
#' hist(x, 100, xlab = "RT [ms]")
#'
#' # Example 2
#' x <- rtDist(n=20000, gaussMean=800, gaussSD=50, expRate=100)
#' hist(x, 100, xlab = "RT [ms]")
#'
#' @export
rtDist <- function(n=10000, gaussMean=600, gaussSD=50, expRate=200) {

  expDist <- stats::rexp(n, 1 / expRate)
  gaussDist <- stats::rnorm(n, gaussMean, gaussSD)
  return(round(expDist + gaussDist - mean(expDist)))

}


#' @title errDist
#'
#' @description Returns a random vector of 0's (correct) and 1's (incorrect) with
#' defined proportions (default = 10\% errors).
#' @param n Number
#' @param proportion Approximate proportion of errors in percentage
#' @return double
#'
#' @examples
#' # Example 1
#' x <- errDist(1000, 10)
#' table(x)
#'
#' @export
errDist <- function(n=10000, proportion = 10) {
  return(ifelse(runif(n) <= proportion / 100, 1, 0))
}

