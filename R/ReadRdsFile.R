#' ReadRdsFile
#'
#' Read content of a rds file.
#'
#' @param fileName Name of file to read. Required.
#' @param ... Additional parameters passed to \code{\link{readRDS}}. Optional.
#'
#' @return data.table object.
#'
#' @examples
#' \dontrun{
#' ReadRdsFile(fileName)
#' }
#'
#' @export
ReadRdsFile <- function(fileName, ...)
{
  stopifnot(!missing(fileName))

  data <- readRDS(file = fileName, ...)

  return(data)
}
