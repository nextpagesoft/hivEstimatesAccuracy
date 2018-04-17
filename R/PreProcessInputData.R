#' PreProcessInputData
#'
#' Pre-processes input data before passing it to adjustment scripts.
#'
#' @param inputData Input data. Required.
#'
#' @return data.table object
#'
#' @examples
#' \dontrun{
#' PreProcessInputData(inputData)
#' }
#'
#' @export
PreProcessInputData <- function(inputData)
{
  stopifnot(!missing(inputData))

  if (is.null(inputData)) {
    return(NULL)
  }

  # Convert all strings to upper case
  colClasses <- sapply(inputData, class)
  charColNames <- names(colClasses[colClasses == "character"])
  inputData[, (charColNames) := lapply(.SD, toupper), .SDcols = charColNames]

  # Cache country codes for countries of Sub-Saharan Africa
  ssaCountryCodes <- countryData[SubRegionName == "Sub-Saharan Africa",
                                 Code]

  # Replace UNKs and BLANKS with NAs
  is.na(inputData) <- inputData == "UNK"
  is.na(inputData) <- inputData == ""

  # Drop unused levels (prior UNKs)
  inputData <- droplevels(inputData)

  # Create Migr variable 1, 2, 3...
  inputData[, Migr := factor(NA, levels = c("Reporting Country", "Other Country", "SSA"))]
  # ...based on RegionOfOrigin if not NA
  inputData[is.na(Migr) & !is.na(RegionOfOrigin),
            Migr := ifelse(RegionOfOrigin == "REPCOUNTRY", 1L,
                           ifelse(!RegionOfOrigin %in% "SUBAFR", 2L,
                                  3L))]
  # ...based on CountryOfBirth if not NA
  inputData[is.na(Migr) & !is.na(CountryOfBirth),
            Migr := ifelse(CountryOfBirth == ReportingCountry, 1L,
                           ifelse(!CountryOfBirth %in% ssaCountryCodes, 2L,
                                  3L))]
  # ...based on CountryOfNationality if not NA
  inputData[is.na(Migr) & !is.na(CountryOfNationality),
            Migr := ifelse(CountryOfNationality == ReportingCountry, 1L,
                           ifelse(!CountryOfNationality %in% ssaCountryCodes, 2L,
                                  3L))]

  # Transform CD4
  inputData[, SqCD4 := sqrt(FirstCD4Count)]

  # AIDS close to diagnosis
  inputData[, AIDS := factor(ifelse(!is.na(DateOfAIDSDiagnosisYear),
                                    ifelse(DateOfAIDSDiagnosisYear - DateOfDiagnosisYear <= 1L, "AIDS-Yes", "AIDS-No"),
                                    "AIDS-No"))]

  # Transform Transmission
  inputData[, Transmission := factor(Transmission)]

  # Imput Gender
  selGenderMissing1 <- inputData[, is.na(Gender) & Transmission %in% "MSM"]
  inputData[selGenderMissing1, Gender := "M"]

  # A single imputation based on categorical year and transmission
  selGenderMissing2 <- inputData[, is.na(Gender)]
  inputDataGender <- inputData[, .(Gender = as.factor(Gender),
                                   DateOfDiagnosisYear = as.factor(DateOfDiagnosisYear),
                                   Transmission)]
  miceImputation <- mice::mice(inputDataGender, m = 1, maxit = 5, printFlag = FALSE)
  inputDataGender <- setDT(mice::complete(miceImputation, action = 1))
  inputData[selGenderMissing2, Gender := inputDataGender$Gender[selGenderMissing2]]

  results <- list(Table = inputData,
                  Artifacts = list(MissGenderReplaced = sum(selGenderMissing1),
                                   MissGenderImputed = sum(selGenderMissing2)))

  return(results)
}
