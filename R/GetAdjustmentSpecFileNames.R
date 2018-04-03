#' GetAdjustmentSpecFileNames
#'
#' Get the list of adjustment file names in a specific folder. By default the search folder is
#' \code{adjustments} folder installed with package \code{\link{hivEstimatesAccuracy}}.
#'
#' @param path Path to the folder with adjustment specifications. Optional.
#'   Default = \code{\link{system.file}("adjustments", package = "hivEstimatesAccuracy")}.
#'
#' @return Character vector of adjustment specification file paths.
#'
#' @examples
#' \dontrun{
#' GetAdjustmentSpecFileNames()
#' }
#'
#' @export
GetAdjustmentSpecFileNames <- function(
  path = system.file("adjustments", package = "hivEstimatesAccuracy"))
{
  adjustmentFileNames <- list.files(path,
                                    pattern = ".R$",
                                    full.names = TRUE)

  adjustmentNames <- sapply(adjustmentFileNames, GetListObject, section = "Name")

  if (anyDuplicated(adjustmentNames) != 0) {
    stop("Adjustments have duplicated names")
  }

  names(adjustmentFileNames) <- adjustmentNames

  return(adjustmentFileNames)
}
