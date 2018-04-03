#' GetAttrMappingStatus
#'
#' Checks attributes mapping and returns detailed information about errors if encountered.
#'
#' @param attrMapping List of attributes mapping between original data and internal representation.
#'   Required.
#'
#' @return list object
#'
#' @examples
#' \dontrun{
#' GetAttrMappingStatus(attrMapping)
#' }
#'
#' @export
GetAttrMappingStatus <- function(attrMapping)
{
  # 1. Find not-mapped attributes
  nonMappedAnalysisAttrs <- names(Filter(IsEmptyString, attrMapping))

  # 2. Find data attributes mapped to multiple analysis attributes
  mappedAnalysisAttrs <- Filter(Negate(IsEmptyString), attrMapping)
  mappedDataAttrs <- unique(unname(sapply(mappedAnalysisAttrs, "[[", 1)))
  multipleMappedDataAttrs <-
    setNames(lapply(mappedDataAttrs, function(mappedDataAttr) {
      multiMappedDataAttr <- names(mappedAnalysisAttrs[mappedAnalysisAttrs == mappedDataAttr])
      if (length(multiMappedDataAttr) != 1) {
        return(multiMappedDataAttr)
      } else {
        return(NULL)
      }
    }), mappedDataAttrs)
  multipleMappedDataAttrs <- Filter(Negate(is.null), multipleMappedDataAttrs)

  # 3. Find if the mapping is valid at all
  if (length(multipleMappedDataAttrs) == 0) {
    valid <- TRUE
  } else {
    valid <- FALSE
  }

  return(list(Valid = valid,
              NonMapped = nonMappedAnalysisAttrs,
              MultipleMapped = multipleMappedDataAttrs))
}
