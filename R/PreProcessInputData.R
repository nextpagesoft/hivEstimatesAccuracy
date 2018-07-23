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
  colClasses <- sapply(sapply(inputData, class), "[[", 1)
  charColNames <- names(colClasses[colClasses == "character"])
  inputData[, (charColNames) := lapply(.SD, toupper), .SDcols = charColNames]

  # Cache country codes for countries of Sub-Saharan Africa
  ssaCountryCodes <- countryData[SubRegionName == "Sub-Saharan Africa",
                                 Code]

  # Replace UNKs and BLANKS with NAs
  for (i in charColNames) {
    inputData[get(i) %chin% c("UNK", ""), (i) := NA]
  }

  # Drop unused levels (prior UNKs)
  inputData <- droplevels(inputData)

  AUSTNZ_countries <-
    c('AU','CC','CX','HM','NF','NZ')

  CAR_countries <-
    c('AG','AI','AW','BB','BL','BM','BQ','BS','BV','CU','CW','DM','DO', 'GD','GP','HT','JM','KN',
      'KY','LC','MF','MQ','MS','PR','SX','TC', 'TT', 'VC','VG','VI')

  CENTEUR_countries <-
    c('AL','BA','BG','CY','CZ','HR','HU','ME','MK','PL','RO','RS','SI', 'SK','TR')

  EASTASIAPAC_countries <-
    c('AS','CK','CN','FJ','HK','JP','KP','KR','MN','MO','NU','PF', 'PG','SB','TK','TO','TV','TW',
      'VU','WF','WS')

  EASTEUR_countries <-
    c('AM','AZ','BY','EE','GE','KG','KZ','LT','LV','MD','RU','TJ','TM', 'UA','UZ')

  LATAM_countries <-
    c('AR','BO','BR','BZ','CL','CO','CR','EC','FK','GF','GS','GT','GY', 'HN','MX','NI','PA','PE',
      'PY','SR','SV','UY','VE')

  NORTHAFRMIDEAST_countries <-
    c('AE','BH','DZ','EG','EH','IQ','JO','KW','LB','LY','MA', 'OM','PS','QA','SA','SD','SS','SY',
      'TN','YE')

  NORTHAM_countries <-
    c('CA','PM','US')

  SOUTHASIA_countries <-
    c('AF','BD','BN','BT','FM','GU','ID','IN','IR','KH','KI','LA','LK', 'MH','MM','MP','MV','MY',
      'NC','NP','NR','PH','PK','PN','PW','SG', 'TH','TL','UM','VN')

  SUBAFR_countries <-
    c('AO','BF','BI','BJ','BW','CD','CF','CG','CI','CM','CV','DJ','ER', 'ET','GA','GH','GM','GN',
      'GQ','GW','IO','KE','KM','LR','LS','MG', 'ML','MR','MU','MW','MZ','NA','NE','NG','RE','RW',
      'SC','SH','SL', 'SN','SO','ST','SZ','TD','TF','TG','TZ','UG','YT','ZA','ZM','ZW')

  UNK_countries <-
    c('AQ')

  WESTEUR_countries <-
    c('AD','AT','AX','BE','CH','DE','DK','EL','ES','FI','FO','FR','GG', 'GI','GL','IE','IL','IM',
      'IS','IT','JE','LI','LU','MC','MT','NL', 'NO','PT','SE','SJ','SM','UK','VA')

  inputData[, RegionOfBirth :=
              ifelse(!is.na(CountryOfBirth) & CountryOfBirth %chin% ReportingCountry,
                     "REPCOUNTRY", NA_character_)]
  inputData[CountryOfBirth %chin% WESTEUR_countries,         RegionOfBirth := "WESTEUR"]
  inputData[CountryOfBirth %chin% CENTEUR_countries,         RegionOfBirth := "CENTEUR"]
  inputData[CountryOfBirth %chin% EASTEUR_countries,         RegionOfBirth := "EASTEUR"]
  inputData[CountryOfBirth %chin% SUBAFR_countries,          RegionOfBirth := "SUBAFR"]
  inputData[CountryOfBirth %chin% EASTASIAPAC_countries,     RegionOfBirth := "EASTASIAPAC"]
  inputData[CountryOfBirth %chin% AUSTNZ_countries,          RegionOfBirth := "AUSTNZ"]
  inputData[CountryOfBirth %chin% SOUTHASIA_countries,       RegionOfBirth := "SOUTHASIA"]
  inputData[CountryOfBirth %chin% NORTHAFRMIDEAST_countries, RegionOfBirth := "NORTHAFRMIDEAST"]
  inputData[CountryOfBirth %chin% NORTHAM_countries,         RegionOfBirth := "NORTHAM"]
  inputData[CountryOfBirth %chin% CAR_countries,             RegionOfBirth := "CAR"]
  inputData[CountryOfBirth %chin% LATAM_countries,           RegionOfBirth := "LATAM"]
  inputData[CountryOfBirth %chin% UNK_countries,             RegionOfBirth := "UNK"]

  inputData[, RegionOfNationality :=
              ifelse(!is.na(CountryOfNationality) & CountryOfNationality %chin% ReportingCountry,
                     "REPCOUNTRY", NA_character_)]
  inputData[RegionOfNationality %chin% WESTEUR_countries,         RegionOfNationality := "WESTEUR"]
  inputData[RegionOfNationality %chin% CENTEUR_countries,         RegionOfNationality := "CENTEUR"]
  inputData[RegionOfNationality %chin% EASTEUR_countries,         RegionOfNationality := "EASTEUR"]
  inputData[RegionOfNationality %chin% SUBAFR_countries,          RegionOfNationality := "SUBAFR"]
  inputData[RegionOfNationality %chin% EASTASIAPAC_countries,     RegionOfNationality := "EASTASIAPAC"]
  inputData[RegionOfNationality %chin% AUSTNZ_countries,          RegionOfNationality := "AUSTNZ"]
  inputData[RegionOfNationality %chin% SOUTHASIA_countries,       RegionOfNationality := "SOUTHASIA"]
  inputData[RegionOfNationality %chin% NORTHAFRMIDEAST_countries, RegionOfNationality := "NORTHAFRMIDEAST"]
  inputData[RegionOfNationality %chin% NORTHAM_countries,         RegionOfNationality := "NORTHAM"]
  inputData[RegionOfNationality %chin% CAR_countries,             RegionOfNationality := "CAR"]
  inputData[RegionOfNationality %chin% LATAM_countries,           RegionOfNationality := "LATAM"]
  inputData[RegionOfNationality %chin% UNK_countries,             RegionOfNationality := "UNK"]

  # Create detailed 'Migrant' variable: FullMigr based on Region Of Origin
  inputData[, FullMigr := RegionOfOrigin]

  # Update if missing with Region of Birth
  inputData[is.na(FullMigr), FullMigr := RegionOfBirth]
  # Update if 'ABROAD' with Region of Birth
  inputData[FullMigr %chin% "ABROAD" & !is.na(RegionOfBirth) & !CountryOfBirth %chin% ReportingCountry,
            FullMigr := RegionOfBirth]

  # Update if missing with Region of Nationality
  inputData[is.na(FullMigr), FullMigr := RegionOfNationality]
  # Update if 'ABROAD' with Region of Nationality
  inputData[FullMigr %chin% "ABROAD" & !is.na(RegionOfNationality) & !CountryOfNationality %chin% ReportingCountry,
            FullMigr := RegionOfNationality]
  inputData[, FullMigr := factor(FullMigr)]

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
