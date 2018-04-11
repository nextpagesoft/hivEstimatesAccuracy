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

  results <- list(Table = inputData,
                  Artifacts = list(MissGenderReplaced = sum(selGenderMissing1),
                                   MissGenderImputed = sum(selGenderMissing2)))

  return(results)
}
