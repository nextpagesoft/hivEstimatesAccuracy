#' PreProcessInputDataForMice
#'
#' Pre-processes input data before passing it to mice adjustment scripts.
#'
#' @param inputData Input data. Required.
#'
#' @return data.table object
#'
#' @examples
#' \dontrun{
#' PreProcessInputDataForMice(inputData)
#' }
#'
#' @export
PreProcessInputDataForMice <- function(inputData)
{
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

  numGenderMissing1 <- sum(selGenderMissing1)
  numGenderMissing2 <- sum(selGenderMissing2)
  numRecords <- nrow(inputData)
  cat(sprintf("%d (and %f%% of total records) of missing gender cases were replaced with \"Male\" because transmission was reported as \"MSM\".\n", numGenderMissing1, numGenderMissing1 / numRecords),
      sprintf("%d (and %f%% of total records) cases with missing gender were imputed through single imputation as this is required for the main multiple imputations procedures.\n", numGenderMissing2, numGenderMissing2 / numRecords),
      sep = "")

  return(inputData)
}
