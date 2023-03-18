.onLoad <- function(libname = find.package("DMCfun"), pkgname = "DMCfun") {

  # CRAN Note avoidance
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
  # adapted from linked-to grattan package from the above link

  utils::globalVariables(
    c(names(DMCfun::flankerData$data),
      names(DMCfun::flankerData$summary),
      names(DMCfun::flankerData$summarySubject),
      names(DMCfun::flankerData$delta),
      names(DMCfun::flankerData$deltaSubject),
      names(DMCfun::flankerData$caf),
      names(DMCfun::flankerData$cafSubject),
      ".",
      "DataSet",
      "Effect",
      "N",
      "Parameters",
      "Trial",
      "accPer",
      "accPer_comp",
      "accPer_incomp",
      "bin",
      "boundary",
      "comp",
      "cond",
      "incomp",
      "nCor",
      "nErr",
      "nTrials",
      "perE",
      "prob",
      "rtC",
      "rtE",
      "rt_comp",
      "rt_incomp",
      "x")
    )
}

.onUnload <- function(libpath) {
  library.dynam.unload("DMCfun", libpath)
}
