#' GetListObject
#'
#' Returns list object stored as text in R script file.
#'
#' @param fileName Name of list file. Required.
#' @param section Name of section of the list to return. If not specified, then
#'   the whole specification is returned, otherwise only the specified section. Optional.
#'   Default = NULL.
#' @param includeFileName Logical indicating to include file name in the returned list as field
#'   "FileName". Optional. Default = TRUE.
#'
#' @return List object.
#'
#' @examples
#' \dontrun{
#' GetListObject(fileName)
#' GetListObject(fileName, section = "Name")
#' GetListObject(fileName, section = "Parameters")
#' }
#'
#' @export
GetListObject <- function(fileName, section = NULL, includeFileName = TRUE)
{
  stopifnot(!missing(fileName))

  # Get entire list
  listObject <- eval(expr = parse(file = fileName))
  if (includeFileName) {
    listObject$FileName <- fileName
  }

  # Return only a specific section of the list
  if (!is.null(section)) {
    if (section %in% names(listObject)) {
      listObject <- listObject[[section]]
    }
  }

  return(listObject)
}
