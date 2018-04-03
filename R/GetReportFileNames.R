#' GetReportFileNames
#'
#' Get the list of report file names in a specific folder. By default the search folder is
#' \code{reports} folder installed with package \code{\link{hivEstimatesAccuracy}}.
#'
#' @param path Path to the folder with adjustment specifications. Optional.
#'   Default = \code{\link{system.file}("reports", package = "hivEstimatesAccuracy")}.
#'
#' @return Character vector of adjustment specification file paths.
#'
#' @examples
#' \dontrun{
#' GetReportFileNames()
#' }
#'
#' @export
GetReportFileNames <- function(
  path = system.file("reports", package = "hivEstimatesAccuracy"))
{
  reportFileNames <- list.files(path,
                                pattern = ".Rmd$",
                                full.names = TRUE)

  reportNames <- sapply(reportFileNames, ReadRmdFrontMatter, section = "name")

  names(reportFileNames) <- reportNames

  return(reportFileNames)
}
