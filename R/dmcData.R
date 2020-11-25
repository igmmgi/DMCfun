#' A summarised dataset: see raw data file flankerDataRaw and dmcObservedData.R
#' This is the summarised Flanker Task data from Ulrich et al. (2015)
#'
#' \itemize{
#'   \item $summary --> Reaction time correct, standard deviation correct, percentage
#'   error, reaction time incorrect, and standard deviation for incorrect trials
#'   for both compatible and incompatible trials
#'   \item $caf --> Proportion correct for compatible and incompatible trials across 5 bins
#'   \item $delta --> Compatible reactions times, incompatible mean reaction times,
#'   mean reaction times, incompatible - compatible reaction times (delta), and
#'   standard deviation + standard error of this difference across 10 bins.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name flankerData
#' @usage flankerData
#' @format dmcob
NULL



#' Raw flanker data from Ulrich et al. (2015)
#'
#' \itemize{
#'   \item Subject Subject number
#'   \item Comp comp vs. incomp
#'   \item RT
#'   \item Error 0 = correct, 1 = error
#' }
#'
#' @docType data
#' @keywords datasets
#' @name flankerDataRaw
#' @usage flankerDataRaw
NULL



#' A summarised dataset: see raw data file simonDataRaw and dmcObservedData.R
#' This is the summarised Simon Task data from Ulrich et al. (2015)
#'
#' \itemize{
#'   \item $summary --> Reaction time correct, standard deviation correct, percentage
#'   error, reaction time incorrect, and standard deviation for incorrect trials
#'   for both compatible and incompatible trials
#'   \item $caf --> Proportion correct for compatible and incompatible trials across
#'   5 bins
#'   \item $delta --> Compatible reactions times, incompatible mean reaction times,
#'   mean reaction times, incompatible - compatible reaction times (delta), and
#'   standard deviation + standard error of this difference across 10 bins.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name simonData
#' @usage simonData
#' @format dmcob
NULL



#' Raw simon data from Ulrich et al. (2015)
#'
#' \itemize{
#'   \item Subject Subject number
#'   \item Comp comp vs. incomp
#'   \item RT
#'   \item Error 0 = correct, 1 = error
#' }
#'
#' @docType data
#' @keywords datasets
#' @name simonDataRaw
#' @usage simonDataRaw
NULL



#' @title createDF: Create a simulated dataframe
#'
#' @description Create dataframe (see also addDataDF)
#'
#' @param nSubjects Number of subjects
#' @param nTrl Number of trials per factor/level for each participant
#' @param design Factors and levels
#'
#' @return dataframe
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



#' @title addDataDF: Add data to a simuluated dataframe
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

    for (i in c(1:length(RT))) {

      fcts_levls <- unlist(strsplit(names(RT[i]),  split = "_"))
      fcts       <- unlist(strsplit(fcts_levls[1], split = ":"))
      levls      <- unlist(strsplit(fcts_levls[2], split = ":"))

      idx <- NULL
      for (fct in c(1:length(fcts))) {
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

    for (i in c(1:length(Error))) {

      fcts_levls <- unlist(strsplit(names(Error[i]), split = "_"))
      fcts       <- unlist(strsplit(fcts_levls[1],   split = ":"))
      levls      <- unlist(strsplit(fcts_levls[2],   split = ":"))

      idx <- NULL
      for (fct in c(1:length(fcts))) {
        idx <- cbind(idx, dat[fcts[fct]] == levls[fct])
      }
      idx <- apply(idx, 1, all)

      dat$bins[idx] <- dplyr::ntile(dat$RT[idx], length(Error[[1]]))
      for (bin in 1:length(Error[[1]])) {
        idx_bin <- dat$bins == bin & idx
        dat$Error[idx_bin] <- errDist(n = sum(idx_bin), Error[[i]][bin])
      }

    }
  }

  if ("bins" %in% names(dat)) {
    dat <- dat[ , -which(names(dat) %in% c("bins"))]
  }

  return(dat)

}



#' @title dmcObservedData: Run standard analyses on observed data
#'
#' @description Basic example analysis script to create data object required
#' for observed data. Example raw *.txt files are flankerData.txt and simonData.txt. There
#' are four critical columns:
#' A column containing subject number
#' A column coding for compatible or incompatible
#' A column with RT (in ms)
#' A column indicating of the response was correct
#' @param dat Text file(s) containing the observed data or an R DataFrame (see createDF/addDataDF)
#' @param nCAF Number of CAF bins. 
#' @param nDelta Number of delta bins. 
#' @param outlier Outlier limits in ms (e.g., c(200, 1200))
#' @param columns Name of required columns DEFAULT = c("Subject", "Comp", "RT", "Error")
#' @param compCoding Coding for compatibility DEFAULT = c("comp", "incomp")
#' @param errorCoding Coding for errors DEFAULT = c(0, 1))
#' @param quantileType Argument (1-9) from R function quantile specifying the algorithm (?quantile)
#' @param delim Single character used to separate fields within a record if reading from external text file.
#' @param skip Number of lines to skip before reading data if reading from external text file.
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
#' # Example 4
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
#' # Example 5
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
                            nCAF = 5,
                            nDelta = 19,
                            outlier = c(200, 1200),
                            columns = c("Subject", "Comp", "RT", "Error"),
                            compCoding = c("comp", "incomp"),
                            errorCoding = c(0, 1),
                            quantileType = 5,
                            delim = "\t",
                            skip = 0) {

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
    names(dat) = c("Subject", "Comp", "RT", "Error")
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

  # aggregate data across trials within subjects
  datSubject <- dat %>%
    dplyr::mutate(outlier = RT <= rtMin | RT >= rtMax) %>%
    dplyr::group_by(Subject, Comp) %>%
    dplyr::summarize(N       = n(),
                     nCor    = sum(Error == 0),
                     nErr    = sum(Error),
                     nOut    = sum(outlier),
                     rtCor   = mean(RT[Error == 0 & outlier == 0]),
                     rtErr   = mean(RT[Error == 1 & outlier == 0]),
                     perErr  = (nErr/(nErr + nCor))*100,
                     .groups = 'drop')

  # aggregate data across subjects
  datAgg <- datSubject %>%
    dplyr::mutate(rtC = rtCor,
                  rtE = rtErr,
                  perE = perErr) %>%
    dplyr::group_by(Comp) %>%
    dplyr::summarize(N        = n(),
                     NCor     = sum(nCor),
                     NErr     = sum(nErr),
                     NOut     = sum(nOut),
                     rtCor    = mean(rtC, na.rm = TRUE),
                     sdRtCor  = sd(rtC, na.rm = TRUE),
                     seRtCor  = sdRtCor/sqrt(N),
                     rtErr    = mean(rtE, na.rm = TRUE),
                     sdRtErr  = sd(rtE, na.rm = TRUE),
                     seRtErr  = sdRtErr/sqrt(N),
                     perErr   = mean(perE, na.rm = TRUE),
                     sdPerErr = sd(perE, na.rm = TRUE),
                     sePerErr = sdPerErr/sqrt(N),
                     .groups  = 'drop')

  # conditional accuracy functions (CAF)
  datSubject_caf <- dat %>%
    dplyr::filter(RT >= rtMin, RT <= rtMax) %>%
    calculateCAF(., nCAF = nCAF)

  datAgg_caf <- datSubject_caf %>%
    dplyr::group_by(Comp, bin) %>%
    dplyr::summarize(accPer  = mean(accPer),
                     .groups = 'drop')

  # DELTA
  datSubject_dec <- dat %>%
    dplyr::filter(Error == 0, RT >= rtMin, RT <= rtMax) %>%
    calculateDelta(., nDelta = nDelta)

  datAgg_dec <- datSubject_dec %>%
    dplyr::mutate(mEffect = meanEffect) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarize(meanComp   = mean(meanComp),
                     meanIncomp = mean(meanIncomp),
                     meanBin    = mean(meanBin),
                     meanEffect = mean(mEffect),
                     sdEffect   = sd(mEffect),
                     seEffect   = sdEffect/sqrt(n()),
                     .groups    = 'drop')

  ##############################################################################
  # save results
  obj <- list()

  # summary
  obj$summarySubject        <- as.data.frame(datSubject[ , c(1, 2, 7, 9, 8)])
  names(obj$summarySubject) <- c("Subject", "Comp", "rtCor", "perErr", "rtErr")
  obj$summary               <- as.data.frame(datAgg[ , c(1, 6, 7, 8, 12, 13, 14, 9, 10, 11)])

  # caf
  obj$cafSubject        <- as.data.frame(datSubject_caf)
  names(obj$cafSubject) <- c("Subject", "Comp", "bin", "accPer")
  obj$caf               <- as.data.frame(datAgg_caf)

  # delta
  obj$deltaSubject        <- as.data.frame(datSubject_dec)
  names(obj$deltaSubject) <- c("Subject", "bin", "meanComp", "meanIncomp", "meanBin", "meanEffect")
  obj$delta               <- as.data.frame(datAgg_dec)

  class(obj) <- "dmcob"

  return(obj)

}

#' @title dmcCombineObservedData: Combine results from dmcObservedData
#'
#' @description Combine observed datasets
#' @param ... Any number of outputs from dmcObservedData
#'
#' @return dmcobs
#'
#' @examples
#' # Example 1
#' datFlanker <- dmcObservedData(flankerDataRaw, nDelta = 9)
#' datSimon <- dmcObservedData(simonDataRaw, nDelta = 9)
#' dat <- dmcCombineObservedData(datFlanker, datSimon)  # combine flanker/simon data
#' plot(dat, figType = "delta", xlimDelta = c(200, 700), 
#'      cols = c("black", "darkgrey"), pchs = c(1, 2), resetPar = FALSE)  
#' legend(200, 0, legend = c("Flanker Task", "Simon Task"), 
#'        col = c("black", "darkgrey"), lty = c(1, 1))
#'
#' @export
dmcCombineObservedData <- function(...) {
  dat <- list(...) 
  class(dat) <- "dmcobs"
  return (dat)
}


#' @title calculateCAF: Calculate conditional accuracy function (CAF).
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
#' @return DataFrame 
#'
#' @examples
#' # Example 1
#' dat <- createDF(nSubjects = 1, nTrl = 100, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(500, 80, 100),
#'                            "Comp_incomp" = c(600, 80, 140)),
#'                  Error = list("Comp_comp"   = c(5, 4,3,2,1),
#'                               "Comp_incomp" = c(20, 8, 6, 4, 2)))
#' caf <- calculateCAF(dat)
#'
#' # Example 2
#' dat <- createDF(nSubjects = 1, nTrl = 100, design = list("Congruency" = c("cong", "incong")))
#' dat <- addDataDF(dat,
#'                  RT = list("Congruency_cong"   = c(500, 80, 100),
#'                            "Congruency_incong" = c(600, 80, 140)),
#'                  Error = list("Congruency_cong"   = c(5, 4,3,2,1),
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
    names(dat) = c("Subject", "Comp", "RT", "Error")
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
    dplyr::mutate(bin = ntile(RT, nCAF)) %>%
    dplyr::group_by(Subject, Comp, bin) %>%
    dplyr::summarize(N       = n(),
                     accPer  = sum(Error == 0)/N,
                     .groups = 'drop')  %>%
    dplyr::group_by(Subject, Comp, bin) %>%
    dplyr::summarize(accPer  = mean(accPer),
                     .groups = 'drop')

    return(dat_caf)

}



#' @title calculateDelta: Calculate delta function
#'
#' @description Calculate delta plot. Here RTs are split into n bins (Default: 5) for compatible and
#' incompatible trials separately. Mean RT is calculated for each condition in each bin then
#' subtracted (incompatible - compatible) to give a compatibility effect (delta) at each bin.
#'
#' @param dat DataFrame with columns containing the participant number, condition
#' compatibility, and RT data (in ms).
#' @param nDelta Number of delta bins. 
#' @param columns Name of required columns Default: c("Subject", "Comp", "RT")
#' @param compCoding Coding for compatibility Default: c("comp", "incomp")
#' @param quantileType Argument (1-9) from R function quantile specifying the algorithm (?quantile)
#'
#' @return DataFrame 
#'
#' @examples
#' # Example 1
#' dat <- createDF(nSubjects = 50, nTrl = 100, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(500, 80, 100),
#'                            "Comp_incomp" = c(600, 80, 140)))
#' delta <- calculateDelta(dat)
#'
#' # Example 2
#' dat <- createDF(nSubject = 50, nTrl = 100, design = list("Congruency" = c("cong", "incong")))
#' dat <- addDataDF(dat,
#'                  RT = list("Congruency_cong"   = c(500, 80, 100),
#'                            "Congruency_incong" = c(600, 80, 140)))
#' head(dat)
#' delta <- calculateDelta(dat, columns = c("Subject", "Congruency", "RT"),
#'                         compCoding = c("cong", "incong"))
#'
#'
#' @export
calculateDelta <- function(dat,
                           nDelta = 19,
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
    names(dat) = c("Subject", "Comp", "RT")
  }

  # create default column values for comp and error coding
  if (any(compCoding != c("comp", "incomp"))) {
    dat$Comp <- ifelse(dat$Comp == compCoding[1], "comp", "incomp")
  }
 
  deltaSeq <- seq(0, 100, length.out = nDelta + 2) 
  deltaSeq <- deltaSeq[2:(length(deltaSeq)-1)]

  dat_delta <- dat %>%
    dplyr::group_by(Subject, Comp) %>%
    dplyr::summarize(bin     = deltaSeq,
                     rt      = quantile(RT, deltaSeq/100, type = quantileType),
                     .groups = 'drop')  %>%
    tidyr::pivot_wider(., id_cols = c("Subject", "bin"), names_from = "Comp", values_from = "rt") %>%
    dplyr::mutate(meanComp   = comp,
                  meanIncomp = incomp,
                  meanBin    = (comp + incomp)/2,
                  meanEffect = (incomp - comp)) %>%
    dplyr::select(-dplyr::one_of("comp", "incomp"))

  return(dat_delta)

}



#' @title rtDist: Create random RT distribution
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
#' hist(x, 100)
#'
#' # Example 2
#' x <- rtDist(n=20000, gaussMean=800, gaussSD=50, expRate=100)
#' hist(x, 100)
#'
#' @export
rtDist <- function(n=10000, gaussMean=600, gaussSD=50, expRate=200) {

  expDist <- stats::rexp(n, 1/expRate)
  gaussDist <- stats::rnorm(n, gaussMean, gaussSD)
  return(round(expDist + gaussDist - mean(expDist)))

}


#' @title errDist: Create random binary vector
#'
#' @description Returns a random vector of 0's (correct) and 1's (incorrect) with
#' defined proportions (default = 10\% errors).
#'
#' @param n Number
#' @param proportion Approximate proportion of errors in percentage
#'
#' @return double
#'
#' @examples
#' # Example 1
#' x <- errDist(1000, 10)
#' table(x)
#'
#' @export
errDist <- function(n=10000, proportion = 10) {
  return(ifelse(runif(n) <= proportion/100, 1, 0))
}

