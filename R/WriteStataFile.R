#' WriteStataFile
#'
#' Write content of a data table (data frame as well) to a Stata file.
#'
#' @param data data.table object to save. Required.
#' @param fileName Name of the saved file. Required.
#' @param ... Additional parameters passed to \code{\link[foreign]{write.dta}}. Optional.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' WriteStataFile(data, fileName)
#' }
#'
#' @export
WriteStataFile <- function(data, fileName, ...)
{
  stopifnot(!missing(data))
  stopifnot(!missing(fileName))

  foreign::write.dta(data, fileName, ...)

  return(NULL)
}
