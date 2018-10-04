#' ReadExcelFile
#'
#' Read content of an Excel file.
#'
#' @param fileName Name of file to read. Required.
#' @param fileType Type of data to read. Required if \code{fileName} has no file extension,
#'   otherwise optional.
#' @param ... Additional parameters passed to \code{\link[readxl]{read_excel}}. Optional.
#'
#' @return data.table object.
#'
#' @examples
#' \dontrun{
#' ReadExcelFile(fileName)
#' }
#'
#' @export
ReadExcelFile <- function(fileName, fileType, ...)
{
  stopifnot(!missing(fileName))

  if (missing(fileType)) {
    fileType <- tolower(tools::file_ext(fileName))
  }

  data <- switch(fileType,
                 "xls"  = data.table::setDT(readxl::read_xls(path = fileName, ...)),
                 "xlsx" = data.table::setDT(readxl::read_xlsx(path = fileName, ...)),
                 {"Unsupported file extension"})

  return(data)
}
