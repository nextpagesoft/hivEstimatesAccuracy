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
#' IsEmptyString("text")
#'
#' @export
IsEmptyString <- function(text) {
  stopifnot(!missing(text))

  return(is.null(text) || text == "" || is.na(text))
}
