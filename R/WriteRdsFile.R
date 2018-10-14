#' WriteRdsFile
#'
#' Write an object to a R rds file.
#'
#' @param data Object to save. Required.
#' @param fileName Name of the saved file. Required.
#' @param ... Additional parameters passed to \code{saveRDS}. Optional.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' WriteRdsFile(data, fileName)
#' }
#'
#' @export
WriteRdsFile <- function(data, fileName, compress = "xz", ...)
{
  stopifnot(!missing(data))
  stopifnot(!missing(fileName))

  saveRDS(data, fileName, compress = compress, ...)

  return(NULL)
}
