#' GetOriginDistribution
#'
#' Get distribution of RegionOfOrigin
#'
#' @param inputData Input data. Required.
#'
#' @return data.table
#'
#' @examples
#' inputData <- data.table::data.table(FullRegionOfOrigin = c("REPCOUNTRY", "SUBAFR", "SUBAFR", "UNK"))
#' GetOriginDistribution(inputData)
#'
#' @export
GetOriginDistribution <- function(inputData)
{
  distr <- inputData[, .(Count = .N), by = FullRegionOfOrigin]
  distr[is.na(FullRegionOfOrigin), FullRegionOfOrigin := "UNK"]
  distr <- distr[order(-Count)]
  distr <- rbind(distr[FullRegionOfOrigin == "REPCOUNTRY"],
                 distr[!FullRegionOfOrigin %chin% c("REPCOUNTRY", "UNK", "OTHER")],
                 distr[FullRegionOfOrigin == "UNK"],
                 distr[FullRegionOfOrigin == "OTHER"])
  return(distr)
}
