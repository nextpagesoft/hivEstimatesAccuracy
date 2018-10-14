#' FormatTime
#'
#' Pretty printing of time in format YYYY/MM/DD HH:MM:SS.
#'
#' @param x \code{\link{POSIXct}} object. Required.
#'
#' @return string
#'
#' @examples
#' x <- Sys.time()
#' FormatTime(x)
#'
#' @export
FormatTime <- function(x)
{
  format(x, "%Y/%m/%d %H:%M:%S")
}
