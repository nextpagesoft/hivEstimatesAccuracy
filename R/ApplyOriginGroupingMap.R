#' ApplyOriginGroupingMap
#'
#' Applies RegionOfOrigin grouping map to the input data
#'
#' @param inputData Input data. Required.
#' @param map Data.table object with ,apping from RegionOfOrigin to GroupOfOrigin. Required.
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
  oldColOrder <- colnames(inputData$Table)
  inputData$Table <- merge(inputData$Table,
                           map,
                           all.x = TRUE,
                           by = "FullMigr",
                           sort = FALSE)
  setcolorder(inputData$Table, c(oldColOrder, "GroupOfOrigin"))

  return(inputData)
}
