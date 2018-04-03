#' AddQuoteMarks
#'
#' Adds quote marks around the passed string.
#'
#' @param text Text to be quoted. Required.
#'
#' @return string
#'
#' @examples
#' AddQuoteMarks("text")
#'
#' @export
AddQuoteMarks <- function(text)
{
  stopifnot(!missing(text))

  return(sprintf("\"%s\"", text))
}
