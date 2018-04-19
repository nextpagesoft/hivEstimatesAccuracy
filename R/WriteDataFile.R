#' WriteDataFile
#'
#' Write content of a data table to file, currently only text file.\cr
#' It is a wrapper around \code{\link{WriteTextFile}}.
#'
#' @param data data.table object to save. Required.
#' @param fileName Name of the saved file. Required.
#' @param ... Additional parameters passed to worker functions. Optional.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' WriteDataFile(data, fileName)
#' }
#'
#' @export
WriteDataFile <- function(data, fileName, ...)
{
  stopifnot(!missing(data))
  stopifnot(!missing(fileName))

  # Get file extension
  fileExtension <- tolower(tools::file_ext(fileName))
  if (fileExtension == "") {
    fileExtension <- "csv"
  }

  switch(fileExtension,
    "csv" = {
      # Default separator
      defSep <- ifelse(Sys.localeconv()[["decimal_point"]] == ",", ";", ",")
      # If separator is provided in the actual argument list (...) then use it.
      # Otherwise, use the default one.
      extraParams <- list(...)
      if ("sep" %in% names(extraParams)) {
        WriteTextFile(data, fileName, ...)
      } else {
        WriteTextFile(data, fileName, sep = defSep, ...)
      }
    },
    "txt" = {
      # Default separator
      defSep <- "\t"
      # If separator is provided in the actual argument list (...) then use it.
      # Otherwise, use the default one.
      extraParams <- list(...)
      if ("sep" %in% names(extraParams)) {
        WriteTextFile(data, fileName, ...)
      } else {
        WriteTextFile(data, fileName, sep = defSep, ...)
      }
    },
    "dta" = {
      WriteStataFile(data, fileName, ...)
    },
    "rds" = {
      WriteRdsFile(data, fileName, ...)
    },
    {stop("Unsupported file extension")}
  )

  return(invisible(NULL))
}
