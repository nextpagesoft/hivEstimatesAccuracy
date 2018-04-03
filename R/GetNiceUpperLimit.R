#' GetNiceUpperLimit
#'
#' Generates a "nice" large number.
#'
#' @param val Vector of numeric values. Required.
#'
#' @return numbeic nice number
#'
#' @examples
#' GetNiceUpperLimit(val = 8)
#' GetNiceUpperLimit(val = c(8, 13))
#'
#' @export
GetNiceUpperLimit <- function(val)
{
  niceVal <- 10^ceiling(log10(val))

  niceVal <- ifelse(val < 0.25 * niceVal, 0.25 * niceVal,
                    ifelse(val < 0.5 * niceVal, 0.5 * niceVal,
                           ifelse(val < 0.75 * niceVal, 0.75 * niceVal,
                                  niceVal)))

  return(niceVal)
}
