#' ReadDataFile
#'
#' Read content of a data file, either Excel or text.\cr
#' It is a wrapper around \code{\link{ReadExcelFile}} and \code{\link{ReadTextFile}}. Appropriate
#' loading function is resolved by the extension of \code{fileName}.
#'
#' @param fileName Name of file to read. Required.
#' @param fileType Type of data to read. Required if \code{fileName} has no file extension,
#'   otherwise optional.
#' @param ... Additional parameters passed to \code{\link{ReadExcelFile}} or
#'   \code{\link{ReadTextFile}}. Optional.
#'
#' @return data.table object.
#'
#' @examples
#' \dontrun{
#' ReadDataFile(fileName)
#' ReadDataFile(fileName, "xlsx")
#' ReadDataFile(fileName, "csv")
#' }
#'
#' @export
ReadDataFile <- function(fileName, fileType, ...)
{
  stopifnot(!missing(fileName))

  localFileName <- fileName

  if (tools::file_ext(fileName) == "zip") {
    zippedFilesList <- as.character(unzip(fileName, list = TRUE)$Name)
    if (length(zippedFilesList) == 1) {
      zippedFile <- utils::unzip(zipfile = fileName,
                                 files = zippedFilesList[1],
                                 exdir = tempdir())

      localFileName <- zippedFile

      on.exit({
        unlink(zippedFile)
      })
    }
  }

  # Get file type
  if (missing(fileType)) {
    fileType <- tolower(tools::file_ext(localFileName))
  }

  if (IsEmptyString(fileType)) {
    stop("Undetected file type")
  }

  # Run appropriate reading function
  data <- switch(fileType,
                 "xls"  = ReadExcelFile(localFileName, fileType, ...),
                 "xlsx" = ReadExcelFile(localFileName, fileType, ...),
                 "txt"  = ReadTextFile(localFileName, colClasses = 'character', ...),
                 "csv"  = ReadTextFile(localFileName, colClasses = 'character', ...),
                 "rds"  = ReadRdsFile(localFileName, ...),
                 {"Unsupported file extension"})

  return(data)
}
