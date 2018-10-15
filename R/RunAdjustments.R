#' RunAdjustments
#'
#' Execute adjustments specified in \code{adjustmentFileNames}.
#'
#' @param data data table object on which adjustments should be applied. Required.
#' @param adjustmentSpecs List of adjustment specifications to execute. Optional. Default = \code{list()}.
#' @param yearRange Numeric cector of length two with lower and upper bound for diagnosis year.
#'   Optional. Default = \code{NULL}.
#'
#' @return data table object after adjustments applied.
#'
#' @examples
#' \dontrun{
#' RunAdjustments(data)
#' }
#'
#' @export
RunAdjustments <- function(data, adjustmentSpecs = list(), yearRange = NULL)
{
  stopifnot(!missing(data))

  # Data table performs many operations by reference.
  # We make a copy of the data to make sure the input object is not changed by the adjustment
  # procedures.
  data <- list(Table = copy(data))
  if (!is.null(yearRange)) {
    data$Table <- data$Table[DateOfDiagnosisYear %between% yearRange]
  }

  results <- list()
  # Process adjustments
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
