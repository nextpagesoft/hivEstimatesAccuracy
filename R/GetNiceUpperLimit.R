#' GetNiceUpperLimit
#'
#' Generates a "nice" large number.
#'
#' @param val Vector of numeric values. Required.
#'
#' @return numeric nice number
#'
#' @examples
#' GetNiceUpperLimit(val = 8)
#' GetNiceUpperLimit(val = -8)
#' GetNiceUpperLimit(val = c(8, 13, 1158, -13232))
#'
#' @export
GetNiceUpperLimit <- function(val)
{
  sgnVal <- sign(val)
  val <- abs(val)

  niceVal <- 10^ceiling(log10(val))

  thresholds <- seq(0.1, 1, 0.05)

  multiplier <- sapply(seq_along(val), function(i) {
    thresholds[val[i] < thresholds * niceVal[i]][1]
  })

  niceVal <- niceVal * multiplier * sgnVal

  return(niceVal)
}
