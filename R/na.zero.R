#' na.zero
#'
#' Sets NAs to zero.
#'
#' @param x A numeric vector. No test that x is numeric.
#'
#' @return Vector of length of x, but with NA's replaced by 0.
#'
#' @examples
#' na.zero(c(1, NA))
#'
#' @export
na.zero <- function(x)
{
  x[is.na(x)] <- 0
  x
}
