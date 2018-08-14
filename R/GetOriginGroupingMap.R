#' GetOriginGroupingMap
#'
#' Get mapping from RegionOfOrigin to GroupOfOrigin
#'
#' @param type Grouping type
#' @param distr Distribution of RegionOfOrigin
#'
#' @return NULL
#'
#' @examples
#' distr <- data.table::data.table(
#'   FullRegionOfOrigin = c("REPCOUNTRY", "SUBAFR"),
#'   Count = c(1536, 2237))
#' GetOriginGroupingMap(
#'   type = "REPCOUNTRY + UNK + 4 most prevalent other regions",
#'   distr = distr
#' )
#'
#' @export
GetOriginGroupingMap <- function(type, distr) {
  map <- switch(
    type,
    "REPCOUNTRY + UNK + OTHER" = ,
    "REPCOUNTRY + UNK + 4 most prevalent other regions" = {
      c(REPCOUNTRY      = "REPCOUNTRY",
        ABROAD          = "OTHER",
        SUBAFR          = "OTHER",
        WESTEUR         = "OTHER",
        CENTEUR         = "OTHER",
        EASTEUR         = "OTHER",
        EASTASIAPAC     = "OTHER",
        AUSTNZ          = "OTHER",
        SOUTHASIA       = "OTHER",
        NORTHAFRMIDEAST = "OTHER",
        NORTHAM         = "OTHER",
        CAR             = "OTHER",
        LATAM           = "OTHER",
        UNK             = "UNK")
      },
    "REPCOUNTRY + UNK + SUBAFR + OTHER" = {
      c(REPCOUNTRY      = "REPCOUNTRY",
        SUBAFR          = "SUBAFR",
        ABROAD          = "OTHER",
        WESTEUR         = "OTHER",
        CENTEUR         = "OTHER",
        EASTEUR         = "OTHER",
        EASTASIAPAC     = "OTHER",
        AUSTNZ          = "OTHER",
        SOUTHASIA       = "OTHER",
        NORTHAFRMIDEAST = "OTHER",
        NORTHAM         = "OTHER",
        CAR             = "OTHER",
        LATAM           = "OTHER",
        UNK             = "UNK")
      },
    stop("Unsupported type")
  )

  map <- as.data.table(map, keep.rownames = TRUE)
  setnames(map, c("FullRegionOfOrigin", "GroupedRegionOfOrigin"))

  if (type == "REPCOUNTRY + UNK + 4 most prevalent other regions") {
    sepRegions <- head(distr[!FullRegionOfOrigin %chin% c("REPCOUNTRY", "UNK"),
                             FullRegionOfOrigin], 3)
    map[FullRegionOfOrigin %chin% sepRegions,
        GroupedRegionOfOrigin := FullRegionOfOrigin]
  }

  map[, GroupedRegionOfOrigin := factor(GroupedRegionOfOrigin,
                                        levels = unique(GroupedRegionOfOrigin))]

  return(map)
}
