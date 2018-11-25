list(
  RecordId = list(
    desciption = "Unique identifier for each record within and across the national surveillance system",
    type = "character",
    defaultValue = NA_character_
  ),
  ReportingCountry = list(
    description = "The country reporting the record",
    type = "character",
    values = union("UNK", countryData$Code),
    defaultValue = NA_character_
  ),
  Age = list(
    description = "Exact age at diagnosis of HIV. Age as a crude number is preferred",
    type = "numeric",
    defaultValue = NA_real_
  ),
  FirstCD4Count = list(
    description = "CD4 cell count at time of diagnosis",
    type = "numeric",
    defaultValue = NA_real_
  ),
  FirstCD4DateYear = list(
    description = "Year of first CD4 cell count at time of diagnosis",
    type = "integer",
    defaultValue = NA_integer_
  ),
  CountryOfBirth = list(
    description = "Country of birth of patient",
    type = "character",
    values = union(c("", "UNK"), countryData$Code),
    defaultValue = NA_character_
  ),
  CountryOfNationality = list(
    description = "Country of nationality of patient",
    type = "character",
    values = union(c("", "UNK"), countryData$Code),
    defaultValue = NA_character_
  ),
  RegionOfOrigin = list(
    description = "Region of origin of patient.",
    type = "character",
    values = c("", "UNK", "ABROAD", "AUSTNZ", "CAR", "CENTEUR", "EASTASIAPAC", "EASTEUR", "EUROPE",
               "LATAM", "NORTHAFRMIDEAST", "NORTHAM", "REPCOUNTRY", "SOUTHASIA", "SUBAFR", "WESTEUR"),
    defaultValue = NA_character_
  ),
  DateOfAIDSDiagnosisYear = list(
    description = "Year of AIDS diagnosis",
    type = "integer",
    defaultValue = NA_integer_
  ),
  DateOfDeathYear = list(
    description = "Year of death",
    type = "integer",
    defaultValue = NA_integer_
  ),
  DateOfDiagnosisYear = list(
    description = "Year of diagnosis",
    type = "integer",
    defaultValue = NA_integer_,
    restrictedValues = NA_integer_
  ),
  DateOfDiagnosisQuarter = list(
    description = "Quarter of diagnosis",
    type = "integer",
    defaultValue = NA_integer_
  ),
  DateOfNotificationYear = list(
    description = "Year of notification",
    type = "integer",
    defaultValue = NA_integer_
  ),
  DateOfNotificationQuarter = list(
    description = "Quarter of notification",
    type = "integer",
    defaultValue = NA_integer_
  ),
  Gender = list(
    description = "Gender",
    type = "character",
    values = c("", "UNK", "F", "M", "O"),
    defaultValue = NA_character_
  ),
  Outcome = list(
    description = "Outcome of case",
    type = "character",
    values = c("", "UNK", "A", "D"),
    defaultValue = NA_character_
  ),
  PlaceOfNotification = list(
    description = "Place of notification",
    type = "character",
    defaultValue = NA_character_
  ),
  PlaceOfResidence = list(
    description = "Place of residence",
    type = "character",
    defaultValue = NA_character_
  ),
  Transmission = list(
    description = "Describes the most probable route of Transmission",
    type = "character",
    values = c("", "UNK", "HAEMO", "HETERO", "IDU", "MSM", "MTCT", "NOSO", "TRANSFU"),
    defaultValue = NA_character_
  )
)
