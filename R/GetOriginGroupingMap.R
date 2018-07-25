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
#' distr <- data.table::data.table(FullMigr = c("REPCOUNTRY", "SUBAFR"), Count = c(1536, 2237))
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
  setnames(map, c("FullMigr", "GroupOfOrigin"))

  if (type == "REPCOUNTRY + UNK + 4 most prevalent other regions") {
    sepRegions <- head(distr[!FullMigr %chin% c("REPCOUNTRY", "UNK"),
                             FullMigr], 3)
    map[FullMigr %chin% sepRegions, GroupOfOrigin := FullMigr]
  }

  return(map)
}
