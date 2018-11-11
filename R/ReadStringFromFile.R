#' ReadStringFromFile
#'
#' Read content of a file as a string
#'
#' @param fileName Name of file to read. Required.
#'
#' @return String
#'
#' @examples
#' \dontrun{
#' ReadStringFromFile(fileName)
#' }
#'
#' @export
ReadStringFromFile <- function(fileName)
{
  fileContent <- readChar(fileName, file.info(fileName)$size)

  return(fileContent)
}
