#' @title addDataDF
#'
#' @description Add simulated ex-gaussian reaction-time (RT) data and
#' binary error (Error = 1, Correct = 0) data to dataframe. This function
#' can be used to create simulated dataframes.
#'
#' @param dat DataFrame created from createDF
#' @param RT Parameters for the call to rtDist function
#' @param Error Parameters for the call to errDist function
#'
#' @return DataFrame
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
#' dat <- createDF(nVP = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat, RT = c(500, 150, 100))
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#'
#' # Example 3: defined RT + Error parameters across conditions
#' dat <- createDF(nVP = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp"), vals = c(500, 80, 100)),
#'                            list(c("Comp:incomp"), vals = c(550, 80, 140))),
#'                  Error = list(list(c("Comp:comp"), vals = c(10, 5)),
#'                             list(c("Comp:incomp"), vals = c(20, 10))))
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#' dat <- dmcObservedData(dat)
#' plot(dat)
#'
#' # Example 4
#' # create dataframe with defined RT + Error parameters across different conditions
#' dat <- createDF(nVP = 50, nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp"), vals = c(500, 150, 150)),
#'                            list(c("Comp:incomp"), vals = c(550, 150, 100))),
#'                  Error = list(list(c("Comp:comp"), vals = c(5, 4, 2, 2, 1)),
#'                             list(c("Comp:incomp"), vals = c(25, 8, 5, 2, 2))))
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#' dat <- dmcObservedData(dat)
#' plot(dat, errorBars = TRUE, errorBarType = "sd")
#
#' @export
addDataDF <- function(dat, RT=NULL, Error=NULL) {

   # reaction time
   if (is.null(RT)) {
     dat$RT <- rtDist(n = nrow(dat))
   } else if (!is.null(RT) & is.double(RT)) {
     dat$RT <- rtDist(n = nrow(dat), RT[1], RT[2], RT[3])
   } else if (!is.null(RT) & is.list(RT)) {

     for (i in c(1:length(RT))) {
       numFactors = unlist(lapply(RT[[i]][1], length)[1])
       idx = NULL
       for (fct in c(1:numFactors)) {
         level <- strsplit(RT[[i]][[1]][fct], split = ":", fixed = TRUE)
         idx   <- cbind(idx, dat[level[[1]][1]] == level[[1]][2])
       }
       idx         <- apply(idx, 1, all)
       vals        <- RT[[i]]$vals
       dat$RT[idx] <- rtDist(n = sum(idx), vals[1], vals[2], vals[3])

     }

   }

  # error rate
  if (is.null(Error)) {
    dat$Error <- errDist(n = nrow(dat))
  } else if (!is.null(Error) & is.double(Error)) {
    dat$Error <- errDist(n = nrow(dat), Error)
  } else if (!is.null(Error) & is.list(Error)) {

    dat <- dat %>%
      dplyr::group_by_(names(dat)[2:(ncol(dat) - 1)]) %>%
      dplyr::mutate(bin = ntile(RT, length(Error[[1]]$vals))) %>%
      as.data.frame()

    for (i in c(1:length(Error[[1]]$vals)))  {
      for (j in c(1:length(Error))) {

        numFactors = unlist(lapply(Error[[j]][1], length)[1])
        idx = NULL
        for (fct in c(1:numFactors)) {
          level <- strsplit(Error[[j]][[1]][fct], split = ":", fixed = TRUE)
          idx   <- cbind(idx, dat[level[[1]][1]] == level[[1]][2] & dat$bin == i)
        }

        idx            <- apply(idx, 1, all)
        vals           <- Error[[j]]$vals
        dat$Error[idx] <- errDist(n = sum(idx), vals[i])
      }
    }

  }

  if ("bin" %in% names(dat)) {
    dat <- dat[ , -which(names(dat) %in% c("bin"))]
  }

  return(dat)

}
