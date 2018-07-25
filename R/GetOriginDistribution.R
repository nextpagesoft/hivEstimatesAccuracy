#' GetOriginDistribution
#'
#' Get distribution of RegionOfOrigin
#'
#' @param inputData Input data. Required.
#'
#' @return data.table
#'
#' @examples
#' inputData <- data.table::data.table(FullMigr = c("REPCOUNTRY", "SUBAFR", "SUBAFR", "UNK"))
#' GetOriginDistribution(inputData)
#'
#' @export
GetOriginDistribution <- function(inputData)
{
  distr <- inputData[, .(Count = .N), by = FullMigr]
  distr[is.na(FullMigr), FullMigr := "UNK"]
  distr <- distr[order(-Count)]
  distr <- rbind(distr[FullMigr == "REPCOUNTRY"],
                 distr[!FullMigr %chin% c("REPCOUNTRY", "UNK", "OTHER")],
                 distr[FullMigr == "UNK"],
                 distr[FullMigr == "OTHER"])
  return(distr)
}
