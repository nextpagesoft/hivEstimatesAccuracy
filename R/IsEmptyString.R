#' IsEmptyString
#'
#' Checks if the passed text is NULL or empty ("") or NA (in that order).
#'
#' @param text Text to be tested. Required.
#'
#' @return string
#'
#' @examples
#' IsEmptyString(NULL)
#' IsEmptyString("")
#' IsEmptyString(NA)
#' IsEmptyString(character())
#' IsEmptyString(5)
#' IsEmptyString("5")
#' IsEmptyString("text")
#'
#' @export
IsEmptyString <- function(text) {
  stopifnot(!missing(text))

  return(!is.character(text) || text == "" || is.na(text) || length(text) == 0L)
}
