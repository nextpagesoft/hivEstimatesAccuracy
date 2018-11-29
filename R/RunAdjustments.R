#' RunAdjustments
#'
#' Execute adjustments specified in \code{adjustmentFileNames}.
#'
#' @param data data table object on which adjustments should be applied. Required.
#' @param adjustmentSpecs List of adjustment specifications to execute. Optional. Default = \code{list()}.
#' @param diagYearRange Numeric vector of length two with lower and upper bound for diagnosis year.
#'   Optional. Default = \code{NULL}.
#' @param notifQuarterRange Numeric vector of length two with lower and upper bound for notification quarter.
#'   Optional. Default = \code{NULL}.
#' @param seed Random seed. Optional. Default = NULL
#'
#' @return data table object after adjustments applied.
#'
#' @examples
#' \dontrun{
#' RunAdjustments(data)
#' }
#'
#' @export
RunAdjustments <- function(data, adjustmentSpecs = list(), diagYearRange = NULL, notifQuarterRange = NULL, seed = NULL)
{
  stopifnot(!missing(data))

  # Data table performs many operations by reference.
  # We make a copy of the data to make sure the input object is not changed by the adjustment
  # procedures.
  data <- list(Table = copy(data))
  if (!is.null(diagYearRange)) {
    data$Table <- data$Table[DateOfDiagnosisYear %between% diagYearRange | is.na(DateOfDiagnosisYear)]
  }
  if (!is.null(notifQuarterRange)) {
    data$Table <- data$Table[NotificationTime %between% notifQuarterRange | is.na(NotificationTime)]
  }

  PreProcessInputDataBeforeAdjustments(data$Table)

  # Process adjustments
  set.seed(seed)
  results <- list()
  i <- 0L
  for (adjustmentSpec in adjustmentSpecs) {
    i <- i + 1L

    caption <- sprintf("%d. %s", i, adjustmentSpec$Name)
    if (!"Key" %in% names(adjustmentSpec)) {
      adjustmentSpec$Key <- caption
    }

    cat("--------------------------------------------------------------------------------\n")
    cat("## Executing adjustment: ", caption, "\n\n", sep = "")

    # Extract parameters for better visibility.
    parameters <- GetParamInfoFromAdjustSpec(adjustmentSpec$Parameters,
                                             infoType = "value")

    cat("# Parameters:\n")
    print(setNames(as.character(parameters),
                   names(parameters)))
    cat("\n")

    cat("# Adjustment text output:\n")

    # Run adjustment function
    output <- adjustmentSpec$AdjustmentFunction(inputData = data$Table,
                                                parameters = parameters)

    data <- list(Table = output$Table,
                 Artifacts = output$Artifacts,
                 Parameters = parameters,
                 RunIdx = i,
                 Name = adjustmentSpec$Name,
                 Type = adjustmentSpec$Type,
                 SubType = adjustmentSpec$SubType,
                 TimeStamp = GetTimeStamp())

    if ("Imputation" %in% colnames(data$Table)) {
      setorderv(data$Table, "Imputation")
    }

    # Store intermediate results for later reference
    results[[adjustmentSpec$Key]] <- data

    cat("\nDone with adjustment:", caption, "\n\n")
  }

  return(results)
}
