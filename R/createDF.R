#' @title createDF
#'
#' @description Create dataframe (see also addDataDF)
#'
#' @param nVP Number of participants
#' @param nTrl Number of trials per factor/level for each participant
#' @param design Factors and levels
#'
#' @return dataframe
#'
#' @examples
#' library(DMCfun)
#'
#' # Example 1
#' dat <- createDF()
#'
#' # Example 2
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#'
#' # Example 3
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp"),
#'                                                    "Side" = c("left", "right", "middle")))
#'
#' @export
createDF <- function(nVP = 20,
                     nTrl = 50,
                     design = list("A" = c("A1", "A2"), "B" = c("B1", "B2"))) {

  dat <-  data.frame(expand.grid(modifyList(design, list(VP = c(1:nVP), Trial = c(1:nTrl))))) 
  return(dat[c("VP", names(design))])

}
