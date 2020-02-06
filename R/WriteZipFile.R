#' WriteZipFile
#'
#' Saves sets of files to a zip file. The file contains multiple folders, one for each
#' member of input list.
#'
#' @param hivModelData List of data.table objects. Required.
#' @param outputFilePath Path to the output file. Required. Default = \code{NULL}.
#' @param ... Additional arguments passed to the internal writing function.
#'
#' @return File name of the saved zip file
#'
#' @examples
#' \dontrun{
#' WriteZipFile(hivModelData)
#' }
#'
#' @export
WriteZipFile <- function(
  hivModelData,
  outputFilePath = NULL,
  ...
) {
  stopifnot(!is.null(outputFilePath))

  tempPath <- file.path(tempdir(), "hivModel")

  dir.create(tempPath, showWarnings = FALSE, recursive = TRUE)
  on.exit({
    unlink(tempPath, recursive = TRUE)
  })

  if (all(sapply(hivModelData, is.data.table))) {
    lapply(names(hivModelData), function(dtName) {
      dt <- hivModelData[[dtName]]
      fileName <- sprintf('%s.csv', dtName)
      WriteTextFile(dt, file.path(tempPath, fileName), ...)
    })
  } else {
    lapply(names(hivModelData), function(iterName) {
      dir.create(file.path(tempPath, iterName), showWarnings = FALSE)
      iterData <- hivModelData[[iterName]]
      lapply(names(iterData), function(dtName) {
        dt <- iterData[[dtName]]
        fileName <- sprintf('%s.csv', dtName)
        WriteTextFile(dt, file.path(tempPath, iterName, fileName), ...)
      })
    })
  }

  hivFileNames <- dir(
    path = tempPath, pattern = ".", recursive = TRUE, full.names = FALSE,
    include.dirs = FALSE
  )

  cachedWD <- getwd()
  setwd(tempPath)
  on.exit({
    setwd(cachedWD)
  }, add = TRUE, after = FALSE)

  tempOutputFilePath <- paste0(tempPath, ".zip")
  zip(tempOutputFilePath, files = hivFileNames)

  if (!is.null(outputFilePath)) {
    file.copy(tempOutputFilePath, outputFilePath, overwrite = TRUE)
    unlink(tempOutputFilePath)
  } else {
    outputFilePath <- tempOutputFilePath
  }

  return(outputFilePath)
}
