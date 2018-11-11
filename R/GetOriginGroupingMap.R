#' GetOriginGroupingMap
#'
#' Get mapping from RegionOfOrigin to GroupOfOrigin
#'
#' @param type Grouping type
#' @param distr Distribution of RegionOfOrigin
#' @param groups Groups
#'
#' @return NULL
#'
#' @examples
#' distr <- data.table::data.table(
#'   FullRegionOfOrigin = c("REPCOUNTRY", "SUBAFR"),
#'   Count = c(1536, 2237))
#' GetOriginGroupingMap(
#'   type = "REPCOUNTRY + UNK + 3 most prevalent regions + OTHER",
#'   distr = distr
#' )
#'
#' @export
GetOriginGroupingMap <- function(type, distr, groups)
{
  # Initialize mapping
  map <- c("UNK", "ABROAD", "AUSTNZ", "CAR", "CENTEUR", "EASTASIAPAC",
           "EASTEUR", "EUROPE", "LATAM", "NORTHAFRMIDEAST", "NORTHAM",
           "REPCOUNTRY", "SOUTHASIA", "SUBAFR", "WESTEUR")
  names(map) <- map

  # Adjust according to type
  switch(
    type,
    "REPCOUNTRY + UNK + OTHER" = ,
    "REPCOUNTRY + UNK + 3 most prevalent regions + OTHER" = {
      map[map %chin% c("ABROAD", "SUBAFR", "WESTEUR", "CENTEUR", "EASTEUR",
                       "EASTASIAPAC", "EUROPE", "AUSTNZ", "SOUTHASIA",
                       "NORTHAFRMIDEAST", "NORTHAM", "CAR", "LATAM")] <- "OTHER"
    },
    "REPCOUNTRY + UNK + SUBAFR + OTHER" = {
      map[map %chin% c("ABROAD", "WESTEUR", "CENTEUR", "EASTEUR", "EASTASIAPAC",
                       "EUROPE", "AUSTNZ", "SOUTHASIA", "NORTHAFRMIDEAST",
                       "NORTHAM", "CAR", "LATAM")] <- "OTHER"
    },
    "Custom" = {
      for (group in groups) {
        map[map %chin% group$Regions] <- group$Name
      }
    },
    stop("Unsupported type")
  )

  map <- as.data.table(map, keep.rownames = TRUE)
  setnames(map, c("FullRegionOfOrigin", "GroupedRegionOfOrigin"))

  if (type == "REPCOUNTRY + UNK + 3 most prevalent regions + OTHER") {
    sepRegions <- head(distr[!FullRegionOfOrigin %chin% c("REPCOUNTRY", "UNK"),
                             FullRegionOfOrigin], 3)
    map[FullRegionOfOrigin %chin% sepRegions,
        GroupedRegionOfOrigin := FullRegionOfOrigin]
  }

  map[, GroupedRegionOfOrigin := factor(GroupedRegionOfOrigin,
                                        levels = unique(GroupedRegionOfOrigin))]

  return(map)
}
