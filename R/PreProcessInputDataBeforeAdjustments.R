#' PreProcessInputDataBeforeAdjustments
#'
#' Pre-processes input data before passing it to adjustment scripts.
#' All changes are applied to the input object by reference.
#' There is no need to assign the result of this function.
#'
#' @param inputData Input data. Required.
#'
#' @return data.table object (invisibly)
#'
#' @examples
#' \dontrun{
#' PreProcessInputDataBeforeAdjustments(inputData)
#' }
#'
#' @export
PreProcessInputDataBeforeAdjustments <- function(inputData)
{
  stopifnot(!missing(inputData))

  if (is.null(inputData)) {
    return(NULL)
  }

  # Support imputing reporting delay -----------------------------------------------

  # Create Min and Max notification time
  inputData[, ":="(
    MinNotificationTime = min(NotificationTime, na.rm = TRUE),
    MaxNotificationTime = max(NotificationTime, na.rm = TRUE)
  ), by = .(ReportingCountry)]

  inputData[is.infinite(MinNotificationTime), MinNotificationTime := NA]
  inputData[is.infinite(MaxNotificationTime), MaxNotificationTime := NA]

  # Create VarX, MaxPossibleDelay
  inputData[, c("VarX", "TweakedVarX", "MaxPossibleDelay", "TweakedMaxPossibleDelay") := {
    # Compute VarX
    varX <- 4 * (NotificationTime - DiagnosisTime)
    varX[varX < 0] <- NA

    # Compute MaxPossibleDelay
    maxPossibleDelay <- ifelse(is.na(DiagnosisTime),
                               4 * (MaxNotificationTime - DateOfDiagnosisYear - 0.25),
                               4 * (MaxNotificationTime - DiagnosisTime))
    maxPossibleDelay <- ifelse(is.na(DateOfDiagnosisYear),
                               4 * (NotificationTime - MinNotificationTime),
                               maxPossibleDelay)
    maxPossibleDelay <- ifelse(is.na(maxPossibleDelay),
                               4 * (MaxNotificationTime - MinNotificationTime),
                               maxPossibleDelay)

    tweakedVarX <- ifelse(varX == 0, 0.01, varX)
    tweakedVarX <- ifelse(tweakedVarX == maxPossibleDelay,
                          maxPossibleDelay - 0.01,
                          tweakedVarX)

    # Tweak MaxPossibleDelay
    tweakedMaxPossibleDelay <- ifelse(maxPossibleDelay == 0,
                                      0.02,
                                      maxPossibleDelay)

    list(varX, tweakedVarX, maxPossibleDelay, tweakedMaxPossibleDelay)
  }]

  return(invisible(inputData))
}
