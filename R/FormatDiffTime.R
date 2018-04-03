#' FormatDiffTime
#'
#' Pretty printing of time difference in format HH:MM:SS
#'
#' @param x \code{\link{difftime}} object. Required.
#'
#' @return string
#'
#' @examples
#' startTime <- Sys.time()
#' endTime <- startTime + 100
#' FormatDiffTime(x = endTime - startTime)
#'
#' @export
FormatDiffTime <- function(x) {
  units(x) <- "secs"
  x <- unclass(x)
  y <- abs(x)
  sprintf("%s%02d:%02d:%02d (HH:MM:SS)",
          ifelse(x < 0, "-", ""), # sign
          y %% 86400 %/% 3600,    # hours
          y %% 3600 %/% 60,       # minutes
          y %% 60 %/% 1)          # seconds
}
