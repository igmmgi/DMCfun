#' @title dmcSimR
#'
#' @description DMC model simulation detailed in  Ulrich, R., Schroeter, H., Leuthold, H., & Birngruber, T. (2015).
#' Automatic and controlled stimulus processing in conflict tasks: Superimposed diffusion processes and delta functions.
#' Cognitive Psychology, 78, 148-174. This function uses Base R and is slow. It is recommended to use the function
#' dmcSim which is a wrapper around the c++ function runDMC.cpp and is much faster.
#'
#' @param amp amplitude of automatic activation
#' @param tau time to peak automatic activation
#' @param aaShape shape parameter of automatic activation
#' @param mu drift rate of controlled processes
#' @param sigma diffusion constant
#' @param bnds +- response barrier
#' @param resMean mean of non-decisional component
#' @param resSD standard deviation of non-decisional component
#' @param nTrl number of trials
#' @param tmax number of time points per trial
#' @param varSP TRUE/FALSE variable starting point
#' @param spShape shape parameter of drift rate
#' @param spLim limit range of distribution of starting point
#' @param varDR TRUE/FALSE variable drift rate
#' @param drShape shape parameter of drift rate
#' @param drLim limit range of distribution of drift rate
#' @param nTrlData Number of trials to plot
#' @param stepDelta Step size for the Delta bins. For example, a step size of 5 would result
#' in 19 CAF bins positioned at 5, 10, 15, ... 85, 90, 95\%.
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would result
#' in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#' @param setSeed TRUE/FALSE If TRUE, set.seed(1) is called
#'
#' @return dmcsim
#'
#' The function returns a list with the relevant results from the simulation. The list
#' is accessed with obj$name with name being one of the following:
#' \item{obj$summary}{summary}
#' \item{obj$delta}{delta}
#' \item{obj$caf}{caf}
#' \item{obj$sim}{sim}
#' \item{obj$trials}{trials}
#' \item{obj$prms}{prms}
#'
#' @examples
#' \dontrun{
#' # Example 1
#' dmc <- dmcSimR()
#' plot(dmc)
#'
#' # Example 2
#' dmc <- dmcSimR(tau = 130)
#' plot(dmc)
#'
#' # Example 3
#' dmc <- dmcSimR(tau = 90)
#' plot(dmc)
#'
#' # Example 4
#' dmc <- dmcSimR(varSP = TRUE)
#' plot(dmc, "delta")
#'
#' # Example 5
#' dmc <- dmcSimR(tau = 130, varDR = TRUE)
#' plot(dmc, "caf")
#'
#' # Example 6
#' dmc <- dmcSimR(stepDelta = 10, stepCAF = 25)
#' plot(dmc)
#'
#' }
#'
#' @export
dmcSimR <- function(amp = 20, tau = 30, aaShape = 2, mu = 0.5, sigma = 4, bnds = 75,
                    resMean = 300, resSD = 30, nTrl = 100000, tmax = 1000,
                    varSP = FALSE, spShape = 3, spLim = c(-75, 75),
                    varDR = FALSE, drShape = 3, drLim = c(0.1, 0.7),
                    nTrlData = 5, stepDelta = 5, stepCAF = 20, setSeed = FALSE) {

  if (setSeed) {
    set.seed(1)
  }

  # hold all results in list to match output from dmcSim/dmcCppR
  dmc <- list()

  # simulation
  tim = seq(1, tmax, 1)
  eq4 = amp * exp(-tim / tau) * (exp(1) * tim / (aaShape - 1) / tau)^(aaShape - 1)

  for (comp in c("comp", "incomp")) {

    sign <- ifelse(comp == "comp", 1, -1)

    # drift rate
    if (!varDR) {
      mu_vec     <- sign * eq4 * ((aaShape - 1) / tim - 1 / tau) + mu
      activation <- t(mu_vec + t((sigma*matrix(rnorm(nTrl*length(tim)), nTrl, length(tim)))))
    } else if (varDR) {
      mu         <- randBeta(nTrl, drShape, drLim)
      mu_vec     <- matrix(rep(sign * eq4 * ((aaShape - 1) / tim - 1/tau), each = nTrl), nrow = nTrl) + mu
      activation <- mu_vec + sigma*matrix(rnorm(nTrl*length(tim)), nTrl, length(tim))
    }

    # variable starting point
    if (varSP) {
      sp <- randBeta(nTrl, spShape, spLim)
      activation[, 1] <- activation[, 1] + sp
    }

    # accumulate activation
    activation <- t(apply(activation, 1, cumsum))

    # find reaction time for each trial
    rt    <- max.col(abs(activation) >= bnds, ties.method = "first")
    rtIdx <- (1:length(rt) - 1) * ncol(activation) + rt
    rt    <- cbind(rt + rnorm(nTrl, resMean, resSD), t(activation)[rtIdx] < 0)

    # calculate caf
    dmc$caf <- rbind(dmc$caf, calculateCAF(rt, stepCAF))

    # calculate delta
    dmc$delta <- rbind(dmc$delta,
                       quantile(rt[, 1], probs = seq(stepDelta, 100 - stepDelta, stepDelta)/100,
                                names = FALSE) )

    # store required simulation results
    dmc[["sim"]][["eq4"]] <- eq4
    dmc[["sim"]][[paste0("activation_", comp)]] <- colMeans(activation)
    for (i in c(1:nTrlData)) {
      dmc[["trials"]][[paste0("trials_", comp)]][[i]] = activation[i, ]
    }
    dmc[["sim"]][[paste0("rts_", comp)]] <- rt[, 1]

    # results summary
    dmc$summary <- rbind(dmc$summary,
                         c(mean(rt[,1][rt[,2] == 0]),
                           sd(rt[,1][rt[,2] == 0]),
                           sum(rt[,2])/nTrl*100,
                           mean(rt[,1][rt[,2] == 1]),
                           sd(rt[,1][rt[,2] == 0])))

  }

  # summary
  dmc$summary <- tibble::as_tibble(dmc$summary)
  colnames(dmc$summary) <- c("rtCor", "sdRtCor", "perErr", "rtErr", "sdRtErr")
  dmc$summary <- tibble::add_column(Comp = c("comp", "incomp"), dmc$summary, .before = TRUE)

  # caf
  nCAF    <- ncol(dmc$caf)
  dmc$caf <- tibble::tibble(accPer = c(dmc$caf[1, ], dmc$caf[2, ]))
  dmc$caf <- tibble::add_column(bin = rep(1:nCAF, each = 1, times = 2), dmc$caf, .before = TRUE)
  dmc$caf <- tibble::add_column(Comp = rep(c("comp", "incomp"), each = nCAF), dmc$caf, .before = TRUE)

  # delta
  nDelta    <- ncol(dmc$delta)
  dmc$delta <- tibble::as_tibble(cbind(dmc$delta[1, ],
                                       dmc$delta[2, ],
                                       (dmc$delta[1, ] + dmc$delta[1, ]) / 2,
                                       (dmc$delta[2, ] - dmc$delta[1, ])))
  colnames(dmc$delta) <- c("meanComp", "meanIncomp", "meanBin", "meanEffect")
  dmc$delta <- tibble::add_column("Bin" = rep(1:nDelta, each = 1, times = 1), dmc$delta, .before = TRUE)

  # just keep average drift rate/starting point across both comp/incomp trials
  dmc$sim$dr_sp[1] <- ifelse(!varDR, mu, mean(mu))
  dmc$sim$dr_sp[2] <- ifelse(!varSP, 0, mean(sp))

  # store parameters used to call function
  dmc$prms = modifyList(as.list(formals(dmcSimR)), as.list(sys.call()))

  class(dmc) <- "dmcsim"

  return(dmc)

}
