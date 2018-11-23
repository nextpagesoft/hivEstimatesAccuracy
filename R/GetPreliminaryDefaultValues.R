#' GetPreliminaryDefaultValues
#'
#' Get preliminary default values for input data columns.
#'
#' @return list of default values
#'
#' @examples
#' GetPreliminaryDefaultValues()
#'
#' @export
GetPreliminaryDefaultValues <- function()
{
  columnSpecs <- GetListObject(system.file("referenceData/requiredColumns.R",
                                           package = "hivEstimatesAccuracy"),
                               includeFileName = FALSE)
  defaultValues <- lapply(columnSpecs, function(columnSpec) {
    defaultValue <- columnSpec$defaultValue
    if (is.null(defaultValue)) {
      return("")
    } else {
      return(defaultValue)
    }
  })

  return(defaultValues)
}
