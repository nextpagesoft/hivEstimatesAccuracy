#' PreProcessInputDataBeforeSummary
#'
#' Pre-processes input data before making data summary and passing it to adjustment scripts.
#'
#' @param inputData Input data. Required.
#' @param seed Random seed. Optional. Default = NULL
#'
#' @return data.table object
#'
#' @examples
#' \dontrun{
#' PreProcessInputDataBeforeSummary(inputData)
#' }
#'
#' @export
PreProcessInputDataBeforeSummary <- function(inputData, seed = NULL)
{
  stopifnot(!missing(inputData))

  if (is.null(inputData)) {
    return(NULL)
  }

  # Convert all strings to upper case
  colClasses <- sapply(sapply(inputData, class), "[[", 1)
  charColNames <- names(colClasses[colClasses == "character"])
  inputData[, (charColNames) := lapply(.SD, toupper), .SDcols = charColNames]

  # Replace UNKs and BLANKS with NAs
  for (colName in charColNames) {
    inputData[get(colName) %chin% c("UNK", ""), (colName) := NA]
  }

  # Merge RegionOfBirth and RegionOfNationality
  inputData[countryData[, .(CountryOfBirth = Code, RegionOfBirth = TESSyCode)],
            RegionOfBirth := RegionOfBirth,
            on = .(CountryOfBirth)]
  inputData[countryData[, .(CountryOfNationality = Code, RegionOfNationality = TESSyCode)],
            RegionOfNationality := RegionOfNationality,
            on = .(CountryOfNationality)]

  inputData[!is.na(CountryOfBirth) & CountryOfBirth %chin% ReportingCountry,
            RegionOfBirth := "REPCOUNTRY"]
  inputData[!is.na(CountryOfNationality) & CountryOfNationality %chin% ReportingCountry,
            RegionOfNationality := "REPCOUNTRY"]

  # Create detailed 'Migrant' variable: FullRegionOfOrigin based on Region Of Origin
  inputData[, FullRegionOfOrigin := RegionOfOrigin]

  # Update if missing with Region of Birth
  inputData[is.na(FullRegionOfOrigin), FullRegionOfOrigin := RegionOfBirth]
  # Update if 'ABROAD' with Region of Birth
  inputData[FullRegionOfOrigin %chin% "ABROAD" &
              !is.na(RegionOfBirth) &
              !CountryOfBirth %chin% ReportingCountry,
            FullRegionOfOrigin := RegionOfBirth]

  # Update if missing with Region of Nationality
  inputData[is.na(FullRegionOfOrigin), FullRegionOfOrigin := RegionOfNationality]
  # Update if 'ABROAD' with Region of Nationality
  inputData[FullRegionOfOrigin %chin% "ABROAD" &
              !is.na(RegionOfNationality) &
              !CountryOfNationality %chin% ReportingCountry,
            FullRegionOfOrigin := RegionOfNationality]

  # Cache country codes for countries of Sub-Saharan Africa
  ssaCountryCodes <- countryData[SubRegionName == "Sub-Saharan Africa",
                                 Code]

  # Create GroupOfOrigin variable 1, 2, 3...
  inputData[, GroupOfOrigin := factor(NA, levels = c("Reporting Country", "Other Country", "SSA"))]
  # ...based on RegionOfOrigin if not NA
  inputData[!is.na(RegionOfOrigin),
            GroupOfOrigin := ifelse(RegionOfOrigin == "REPCOUNTRY", 1L,
                                    ifelse(!RegionOfOrigin %chin% "SUBAFR", 2L,
                                           3L))]
  # ...based on CountryOfBirth if not NA
  inputData[is.na(GroupOfOrigin) & !is.na(CountryOfBirth),
            GroupOfOrigin := ifelse(CountryOfBirth == ReportingCountry, 1L,
                                    ifelse(!CountryOfBirth %chin% ssaCountryCodes, 2L,
                                           3L))]
  # ...based on CountryOfNationality if not NA
  inputData[is.na(GroupOfOrigin) & !is.na(CountryOfNationality),
            GroupOfOrigin := ifelse(CountryOfNationality == ReportingCountry, 1L,
                                    ifelse(!CountryOfNationality %chin% ssaCountryCodes, 2L,
                                           3L))]

  # Transform CD4
  inputData[, SqCD4 := sqrt(FirstCD4Count)]

  # AIDS close to diagnosis
  inputData[, AIDS := factor(ifelse(!is.na(DateOfAIDSDiagnosisYear),
                                    ifelse(DateOfAIDSDiagnosisYear - DateOfDiagnosisYear <= 1L,
                                           "AIDS-Yes", "AIDS-No"),
                                    "AIDS-No"))]

  # Imput Gender
  selGenderMissing <- inputData[, is.na(Gender)]
  selGenderReplaced <- selGenderMissing & inputData$Transmission %chin% "MSM"
  selGenderImputed <- selGenderMissing & !selGenderReplaced
  # If Gender missing and Transmission is MSM, then set Male gender
  inputData[selGenderReplaced, Gender := "M"]
  # A single imputation based on categorical year and transmission
  if (any(selGenderImputed)) {
    inputDataGender <- inputData[, .(Gender = as.factor(Gender),
                                     DateOfDiagnosisYear = as.factor(DateOfDiagnosisYear),
                                     Transmission = Transmission)]
    set.seed(seed)
    miceImputation <- suppressWarnings(mice::mice(inputDataGender, m = 1, maxit = 5, printFlag = TRUE))
    inputDataGender <- setDT(mice::complete(miceImputation, action = 1))
    inputData[selGenderImputed, Gender := inputDataGender$Gender[selGenderImputed]]
  }

  # Create helper columns for filtering data on diagnosis and notification time
  inputData[, ":="(
    NotificationTime = DateOfNotificationYear + 1/4 * DateOfNotificationQuarter,
    DiagnosisTime = DateOfDiagnosisYear + 1/4 * DateOfDiagnosisQuarter
  )]

  # Transform columns to factor
  inputData[, Gender := factor(Gender)]
  inputData[, Transmission := factor(Transmission)]

  results <- list(Table = inputData,
                  Artifacts = list(MissGenderReplaced = sum(selGenderReplaced),
                                   MissGenderImputed = sum(selGenderImputed)))

  return(results)
}
