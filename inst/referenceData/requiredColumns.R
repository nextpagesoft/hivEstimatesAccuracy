list(
  RecordId = list(
    desciption = "Unique identifier for each record within and across the national surveillance system",
    type = "character"
  ),
  ReportingCountry = list(
    description = "The country reporting the record",
    type = "character",
    values = union("UNK", countryData$Code)
  ),
  Age = list(
    description = "Exact age at diagnosis of HIV. Age as a crude number is preferred",
    type = "numeric"
  ),
  FirstCD4Count = list(
    description = "CD4 cell count at time of diagnosis",
    type = "numeric"
  ),
  FirstCD4DateYear = list(
    description = "Year of first CD4 cell count at time of diagnosis",
    type = "integer"
  ),
  CountryOfBirth = list(
    description = "Country of birth of patient",
    type = "character",
    values = union(c("", "UNK"), countryData$Code)
  ),
  CountryOfNationality = list(
    description = "Country of nationality of patient",
    type = "character",
    values = union(c("", "UNK"), countryData$Code)
  ),
  RegionOfOrigin = list(
    description = "Region of origin of patient.",
    type = "character",
    values = c("", "UNK", "Unk", "ABROAD", "AUSTNZ", "CAR", "CENTEUR", "EASTASIAPAC", "EASTEUR", "EUROPE", "LATAM",
               "NORTHAFRMIDEAST", "NORTHAM", "REPCOUNTRY", "SOUTHASIA", "SUBAFR", "WESTEUR")
  ),
  DateOfAIDSDiagnosisYear = list(
    description = "Year of AIDS diagnosis",
    type = "integer"
  ),
  DateOfDeathYear = list(
    description = "Year of death",
    type = "integer"
  ),
  DateOfDiagnosisYear = list(
    description = "Year of diagnosis",
    type = "integer"
  ),
  DateOfDiagnosisQuarter = list(
    description = "Quarter of diagnosis",
    type = "integer"
  ),
  DateOfNotificationYear = list(
    description = "Year of notification",
    type = "integer"
  ),
  DateOfNotificationQuarter = list(
    description = "Quarter of notification",
    type = "integer"
  ),
  Gender = list(
    description = "Gender",
    type = "character",
    values = c("", "UNK", "F", "M", "O")
  ),
  Outcome = list(
    description = "Outcome of case",
    type = "character",
    values = c("", "UNK", "A", "D")
  ),
  PlaceOfNotification = list(
    description = "Place of notification",
    type = "character"
  ),
  PlaceOfResidence = list(
    description = "Place of residence",
    type = "character"
  ),
  Transmission = list(
    description = "Describes the most probable route of Transmission",
    type = "character",
    values = c("", "UNK", "HAEMO", "HETERO", "IDU", "MSM", "MTCT", "NOSO", "TRANSFU")
  )
)
