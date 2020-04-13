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
#' @return DataFrame with RT and Error columns
#'
#' @examples
#' library(DMCfun)
#'
#' # Example 1: default dataframe
#' dat <- createDF()
#' dat <- addDataDF(dat)
#' hist(dat$RT, 100)
#' table(dat$Error)
#'
#' # Example 2: defined overall RT parameters
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat, RT = c(500, 150, 100))
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#'
#' # Example 3: defined RT + Error parameters across conditions
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
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
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral")))
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
#' dat <- createDF(nVP = 50, nTrl = 50,
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
#
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


