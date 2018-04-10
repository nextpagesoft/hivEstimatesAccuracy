#' RunAdjustments
#'
#' Execute adjustments specified in \code{adjustmentFileNames}.
#'
#' @param data data table object on which adjustments should be applied. Required.
#' @param adjustmentSpecs List of adjustment specifications to execute. Optional. Default = \code{list()}.
#'
#' @return data table object after adjustments applied.
#'
#' @examples
#' \dontrun{
#' RunAdjustments(data)
#' }
#'
#' @export
RunAdjustments <- function(data, adjustmentSpecs = list())
{
  stopifnot(!missing(data))

  # Data table performs many operations by reference.
  # We make a copy of the data to make sure the input object is not changed by the adjustment
  # procedures.
  data <- list(Table = copy(data),
               Parameters = list())

  results <- list()
  # Process adjustments
  i <- 0
  for (adjustmentSpec in adjustmentSpecs) {
    i <- i + 1

    caption <- paste(i, adjustmentSpec$Name, sep = ".")
    if (!"Key" %in% names(adjustmentSpec)) {
      adjustmentSpec$Key <- caption
    }

    cat("--------------------------------------------------------------------------------\n")
    cat("### Executing adjustment: ", caption, "\n\n", sep = "")

    # Extract parameters for better visibility.
    parameters <- GetParamInfoFromAdjustSpec(adjustmentSpec$Parameters,
                                             infoType = "value")

    cat("## Parameters:\n")
    print(parameters)

    cat("## Adjustment text output:\n")

    # Run adjustment function
    output <- adjustmentSpec$AdjustmentFunction(inputData = data$Table,
                                                parameters = parameters)

    data <- list(Table = output$Table,
                 Artifacts = output$Artifacts,
                 Parameters = parameters,
                 RunIdx = i,
                 Type = adjustmentSpec$Type,
                 SubType = adjustmentSpec$SubType)

    # Store intermediate results for later reference
    results[[adjustmentSpec$Key]] <- data

    cat("\nDone with adjustment:", caption, "\n\n")
  }

  return(results)
}
