#' ApplyOriginGroupingMap
#'
#' Applies RegionOfOrigin grouping map to the input data
#'
#' @param inputData Input data. Required.
#' @param map Data.table object with mapping from RegionOfOrigin to
#'   GroupOfOrigin. Required.
#'
#' @return inputData
#'
#' @examples
#' \dontrun{
#' ApplyOriginGroupingMap(inputData, map)
#' }
#'
#' @export
ApplyOriginGroupingMap <- function(inputData, map)
{
  data <- copy(inputData$Table)

  data[map,
       GroupedRegionOfOrigin := GroupedRegionOfOrigin,
       on = "FullRegionOfOrigin"]

  data[, GroupedRegionOfOrigin := droplevels(GroupedRegionOfOrigin)]
  inputData$Table <- data

  return(inputData)
}
