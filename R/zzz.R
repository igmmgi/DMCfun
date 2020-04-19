.onLoad <- function(libname = find.package("DMCfun"), pkgname = "DMCfun") {

  # CRAN Note avoidance
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
  # adapted from linked-to grattan package from the above webpage

    utils::globalVariables(
      c("RT", "rts", "errs", "VP", "Comp", "Error", "nErrVP", "nCorVP", "nOutVP", "rtCorVP",
        "sdRtCor", "accPerVP", "rt_VP", "comp", "incomp", "deltaCI", "meanCI",
        "sdEffect", "N", "rtErrVP", "sdRtErr", "perErrVP", "sdPerErr", "bin", "meanBinVP",
        "meanCompVP", "meanEffectVP", "meanIncompVP", ".", "Bin", "rtCor", "perErr",
        "rtErr", "meanComp", "meanIncomp", "meanBin", "meanEffect", "accPer", "i", 
        "amp", "tau", "mu", "bnds", "resMean", "resSD", "aaShape", "spShape", "sigm", "RMSE")
      )
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("DMCfun", libpath)
}
