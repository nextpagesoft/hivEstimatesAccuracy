#' ApplyAttributesMapping
#'
#' Applies attributes mapping to original data
#'
#' @param originalData Original data. Required.
#' @param attrMapping List of attributes mapping between original data and internal representation.
#'   Required.
#' @param defaultValues List of default values for non-mapped columns
#'
#' @return data.table
#'
#' @examples
#' \dontrun{
#' ApplyAttributesMapping(originalData, attrMapping)
#' }
#'
#' @export
ApplyAttributesMapping <- function(originalData, attrMapping, defaultValues)
{
  stopifnot(!missing(originalData))
  stopifnot(!missing(attrMapping))

  # Check attributes mapping
  attrMappingStatus <- GetAttrMappingStatus(attrMapping)
  if (!attrMappingStatus$Valid) {
    return(NULL)
  }

  # Deal with mapped attributes
  nonMappedAttributes <- Filter(function(x) IsEmptyString(x), attrMapping)
  mappedAttributes <- Filter(function(x) !IsEmptyString(x), attrMapping)

  oldColNames <- unname(sapply(mappedAttributes, "[[", 1))
  newColNames <- names(mappedAttributes)

  outputData <- originalData[, ..oldColNames]
  setnames(outputData, oldColNames, newColNames)

  # Deal with non-mapped attributes
  for (nonMappedAttributeName in names(nonMappedAttributes)) {
    outputData[, (nonMappedAttributeName) := defaultValues[[nonMappedAttributeName]]]
  }

  # Ensure columns types
  columnSpecs <- GetListObject(system.file("referenceData/requiredColumns.R",
                                           package = "hivEstimatesAccuracy"),
                               includeFileName = FALSE)

  ConvertDataTableColumns(outputData, sapply(columnSpecs, "[[", "type"))

  setcolorder(outputData, names(attrMapping))

  return(outputData)
}
