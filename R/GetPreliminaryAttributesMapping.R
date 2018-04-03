#' GetPreliminaryAttributesMapping
#'
#' Gets attributes mappings.
#'
#' @param origData Original data. Required.
#'
#' @return list with attributes mapping
#'
#' @examples
#' \dontrun{
#' GetPreliminaryAttributesMapping(origData)
#' }
#'
#' @export
GetPreliminaryAttributesMapping <- function(origData)
{
  stopifnot(!missing(origData))

  # Get required columns
  columnSpecs <- GetListObject(system.file("referenceData/requiredColumns.R",
                                           package = "hivEstimatesAccuracy"),
                               includeFileName = FALSE)

  # Get column mapping
  attrMapping <- sapply(names(columnSpecs), function(x) NULL)
  origColNames <- names(origData)
  for (requiredColumnName in names(columnSpecs)) {
    # Fuzzy string matching
    bestMatchColumn <-
      agrep(requiredColumnName, origColNames,
            ignore.case = TRUE, value = TRUE, max.distance = 0)[1]
    # Remove matched column from searching in the next step.
    if (!is.na(bestMatchColumn)) {
      origColNames <- setdiff(origColNames, requiredColumnName)
      attrMapping[[requiredColumnName]] <- bestMatchColumn
    }
  }

  return(attrMapping)
}
