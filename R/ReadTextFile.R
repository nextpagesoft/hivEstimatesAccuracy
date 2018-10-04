#' ReadTextFile
#'
#' Read content of a (tab delimited: see parameters) text file.
#'
#' @param fileName Name of file to read. Required.
#' @param ... Additional parameters passed to \code{\link[data.table]{fread}}. Optional.
#'
#' @return data.table object.
#'
#' @examples
#' \dontrun{
#' ReadTextFile(fileName)
#' }
#'
#' @export
ReadTextFile <- function(fileName, ...)
{
  stopifnot(!missing(fileName))

  data <- data.table::fread(input = fileName, ...)

  return(data)
}
