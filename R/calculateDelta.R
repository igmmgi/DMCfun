#' @title calculateDelta
#'
#' @description Calculate delta plot. Here RTs are split into n bins (Default: 5) for compatible and
#' incompatible trials separately. Mean RT is calculated for each condition in each bin then 
#' subtracted (incompatible - compatible) to give a compatibility effect (delta) at each bin.
#'
#' @param dat DataFrame with columns containing the participant number, condition 
#' compatibility, and RT data (in ms).  
#' @param stepDelta Step size for the Delta bins. For example, a step size of 5 would result
#' in 19 CAF bins positioned at 5, 10, 15, ... 85, 90, 95\%.
#' @param columns Name of required columns Default: c("VP", "Comp", "RT")
#' @param compCoding Coding for compatibility Default: c("comp", "incomp")
#' @param quantileType Argument (1-9) from R function quantile specifying the algorithm (?quantile)
#'
#' @return DataFrame (tibble)
#'
#' @examples
#' # Example 1
#' dat <- createDF(nVP = 50, nTrl = 100, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_comp"   = c(500, 80, 100),
#'                            "Comp_incomp" = c(600, 80, 140)),
#'                  Error = list("Comp_comp"   = c(5, 4,3,2,1),
#'                               "Comp_incomp" = c(20, 8, 6, 4, 2)))
#' delta <- calculateDelta(dat)
#'
#' @export
calculateDelta <- function(dat, 
                           stepDelta = 5, 
                           columns = c("VP", "Comp", "RT"),
                           compCoding = c("comp", "incomp"),
                           quantileType = 5) {
  
  deltaSeq <- seq(stepDelta, 100, stepDelta)
  deltaSeq <- deltaSeq[!deltaSeq %in% 100]
  
  dat_delta <- dat %>%
    dplyr::group_by(VP, Comp) %>%
    dplyr::do(tibble::as_tibble(t(quantile(.$RT, deltaSeq/100, type = quantileType))))  %>%
    setNames(c("VP", "Comp", seq(1, length(deltaSeq)))) %>%
    tidyr::gather(bin, rt_VP, -c("VP", "Comp")) %>%
    tidyr::spread(Comp, rt_VP) %>%
    dplyr::mutate(meanCompVP   = comp,
                  meanIncompVP = incomp,
                  meanBinVP    = (comp + incomp)/2,
                  meanEffectVP = (incomp - comp)) %>%
    dplyr::select(-dplyr::one_of("comp", "incomp")) %>%
    dplyr::mutate(bin = as.integer(bin)) %>%
    dplyr::arrange(VP, bin)
  
  return(dat_delta)
  
}
