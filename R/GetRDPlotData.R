#' GetRDPlotData
#'
#' Get plot data for reporting delay adjustment artifacts.
#'
#' @param data Data table object. Required.
#' @param by Character vector of index columns. Required.
#'
#' @return Data table object
#'
#' @examples
#' \dontrun{
#' GetRDPlotData(data, by)
#' }
#'
#' @export
GetRDPlotData <- function(data, by) {
  # Number of imputations in the data (at least one - dummy)
  nimp <- data[, length(unique(Imputation)) - 1]

  # Aggregatea
  data <- data[, .(Count = sum(Count),
                   EstCount = sum(EstCount),
                   EstCountVar = sum(EstCountVar)),
               by = by]

  # Prepare records for reported data
  dataRep <- data[Source == "Reported"]
  dataRep[, Imputation := NULL]

  # Combine estimates obtained for each imputation
  dataImp <- data[Source == "Imputed",
                  .(Count = mean(Count),
                    EstCount = mean(EstCount),
                    EstCountVarWi = mean(EstCountVar),
                    EstCountVarBe = na.zero(var(EstCount))),
                  by = setdiff(by, "Imputation")]
  dataImp[, ":="(
    EstCountVar = EstCountVarWi + (1 + 1/nimp) * EstCountVarBe,
    EstCountVarWi = NULL,
    EstCountVarBe = NULL
  )]

  # Combine both data sets again
  data <- rbind(dataRep,
                dataImp,
                use.names = TRUE)
  data[, ":="(
    LowerEstCount = EstCount - 1.96 * sqrt(EstCountVar),
    UpperEstCount = EstCount + 1.96 * sqrt(EstCountVar)
  )]

  return(data)
}
