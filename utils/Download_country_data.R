conn <- url("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv")
countryData <- read.csv(conn, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
data.table::setDT(countryData)

countryData <- countryData[, .(official_name_en, `ISO3166.1.Alpha.2`, `UNTERM.English.Formal`,
                               Continent, `Region.Name`, `Sub.region.Name`)]

data.table::setnames(countryData,
                     c("Name", "Code", "FormalName", "ContinentCode", "RegionName", "SubRegionName"))

countryData <- countryData[!is.na(Code)]

countryData[Code == "TW", ":="(
  Name = "Taiwan",
  FormalName = "Republic of China",
  RegionName = "Asia",
  SubRegionName = "Eastern Asia"
)]

countryData[Code == "GR", ":="(
  Code = "EL"
)]

countryData[Code == "GB", ":="(
  Code = "UK"
)]

save(countryData,
     file = "D:/_REPOSITORIES/hivEstimatesAccuracy/data/countryData.rda",
     compress = "xz")
