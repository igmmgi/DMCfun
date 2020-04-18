#' @title addErrorBars
#'
#' @description Add error bars to current plot (uses base arrows function).
#'
#' @param xpos x-position of data-points
#' @param ypos y-position of data-points
#' @param errorSize +- size of error bars
#' @param arrowSize Width of the errorbar arrow
#'
#' @return Plot
#'
#' @examples
#' # Example 1
#' plot(c(1, 2), c(450, 500), xlim = c(0.5, 2.5), ylim = c(400, 600), type = "o")
#' addErrorBars(c(1, 2), c(450, 500), errorSize = c(20, 20))
#'
#' # Example 2
#' plot(c(1, 2), c(450, 500), xlim = c(0.5, 2.5), ylim = c(400, 600), type = "o")
#' addErrorBars(c(1, 2), c(450, 500), errorSize = c(20, 40), arrowSize = 0.2)
#'
#' @export
addErrorBars <- function(xpos, ypos, errorSize, arrowSize = 0.1) {
  errBars <- as.data.frame(cbind(xpos, ypos, errorSize))
  with(errBars, arrows(xpos, ypos - errorSize,
                       xpos, ypos + errorSize,
                       arrowSize, 90, 3))
}
