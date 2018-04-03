#' WriteTextFile
#'
#' Write content of a data table (data frame as well) to a text file.
#'
#' @param data data.table object to save. Required.
#' @param fileName Name of the saved file. Required.
#' @param ... Additional parameters passed to \code{\link[data.table]{fwrite}}. Optional.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' WriteTextFile(data, fileName)
#' }
#'
#' @export
WriteTextFile <- function(data, fileName, ...)
{
  stopifnot(!missing(data))
  stopifnot(!missing(fileName))

  data.table::fwrite(data, fileName, ...)

  return(NULL)
}
