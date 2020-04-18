#' @export
`[.dmclist` <- function(x, i, ...) {
  y = .subset(x, i)
  attributes(y) = attributes(x)
  return(y)
}
