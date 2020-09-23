.onLoad <- function(libname = find.package("DMCfun"), pkgname = "DMCfun") {

  # CRAN Note avoidance
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
  # adapted from linked-to grattan package from the above webpage

  utils::globalVariables(
    c("Bin",  "N",  "RMSE", "aaShape",  "accPer",  "amp",  "bin",  "bnds", 
      "comp",  "deltaCI",  "drc",  "i",  "incomp",  "meanBin", "meanCI", "meanComp", "mEffect", "meanEffect", 
      "meanIncomp", ".",  "perErr",  "resMean",  "resSD",  "rtCor",  "rtErr",  "sdEffect",  "sdPerErr", 
      "sdRtCor",  "sdRtErr",  "sigm",  "spShape",  "tau",  "Comp",  "Error",  "RT",  "Subject", 
      "errs",  "nCor",  "nErr",  "nOut", "rtC", "rtE", "perE", "rtCor", "rts") 
  )
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("DMCfun", libpath)
}
