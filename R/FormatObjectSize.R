#' FormatObjectSize
#'
#' Formats object size to human readable format.
#'
#' @param size Argument name. Required.
#'
#' @return string
#'
#' @examples
#' FormatObjectSize(size = 100)
#' FormatObjectSize(size = 50000)
#' FormatObjectSize(size = 4000000)
#'
#' @export
FormatObjectSize <- function(size)
{
  stopifnot(!missing(size))

  # Get handle of an unexported function (otherwise CRAN checks give warning)
  format.object_size <- get("format.object_size", envir = asNamespace("utils"))

  formattedSize <- format.object_size(size, "auto")

  return(formattedSize)
}
