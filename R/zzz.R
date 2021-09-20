.onLoad <- function(libname = find.package("DMCfun"), pkgname = "DMCfun") {

  # CRAN Note avoidance
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
  # adapted from linked-to grattan package from the above link

  utils::globalVariables(
    c(names(DMCfun::flankerData$data),
      names(DMCfun::flankerData$summary),
      names(DMCfun::flankerData$delta),
      names(DMCfun::flankerData$caf),
      ".", "Effect", "aaShape", "accPer", "amp", "bnds", "comp",
      "cost", "drc", "effect", "incomp", "N", "nCor", "nErr", "per", "perSlow",
      "perE", "resMean", "resSD", "rtC", "rtE", "sigm", "spBias", "spShape", "tau",
      "nTrials", "prob", "boundary", "outlier", "Subject", "Error", "RT")
  )

}

.onUnload <- function(libpath) {
  library.dynam.unload("DMCfun", libpath)
}
