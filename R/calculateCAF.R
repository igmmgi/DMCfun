#' @title calculateCAF
#'
#' @description Calculate conditional accuracy function (CAF) on DataFrame
#'
#' @param dat DataFrame with a column containing the participant number, a column coding 
#' compatible vs. incompatible, a column with the RT data (in ms) and a column coding
#' if the trial was an error (correct = 0, error = 1) 
#' @param stepCAF Step size for the CAF bins. For example, a step size of 20 would
#' result in 5 CAF bins centered on 10, 30, 50, 70, and 90\%.
#' @param columns Name of required columns DEFAULT = c("VP", "Comp", "RT", "Error")
#' @param compCoding Coding for compatibility DEFAULT = c("comp", "incomp")
#' @param errorCoding Coding for errors DEFAULT = c(0, 1))
#'
#' @return DataFrame (tibble)
#'
#' @examples
#' library(DMCfun)
#'
#' # Example 1
#' dat <- createDF(nVP = 50, nTrl = 100, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(500, 80, 100),
#'                            "Comp_incomp" = c(600, 80, 140)),
#'                  Error = list("Comp_comp"   = c(5, 4,3,2,1),
#'                               "Comp_incomp" = c(20, 8, 6, 4, 2)))
#' caf <- calculateCAF(dat)
#'
#' @export
calculateCAF <- function(dat, 
                         stepCAF = 20, 
                         columns = c("VP", "Comp", "RT", "Error"),
                         compCoding = c("comp", "incomp"),
                         errorCoding = c(0, 1)) {
                              
  # conditional accuracy functions (CAF)
  dat_caf <- dat %>%
    dplyr::group_by(VP, Comp) %>%
    dplyr::mutate(bin = ntile(RT, (100/stepCAF))) %>%
    dplyr::group_by(VP, Comp, bin) %>%
    dplyr::summarize(N        = n(),
                     accPerVP = sum(Error == 0)/N)  %>%
    dplyr::group_by(VP, Comp, bin) %>%
    dplyr::summarize(accPerVP = mean(accPerVP))

    return(dat_caf)

}
