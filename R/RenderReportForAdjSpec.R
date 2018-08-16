#' RenderReportForAdjSpec
#'
#' Renders html fragment from RMarkdown document.
#'
#' @param adjustmentSpec Adjustment specification. Required.
#' @param fileNameSuffix Report file suffix. Base name is taken from the adjustment spec file name.
#'   Optional. Default = "".
#' @param params Parameters passed to the report. Optional. Default = NULL.
#'
#' @return string with the rendered report
#'
#' @examples
#' \dontrun{
#' RenderReportForAdjSpec(adjustmentSpec, fileNameSuffix, parameters)
#' }
#'
#' @export
RenderReportForAdjSpec <- function(adjustmentSpec, fileNameSuffix = "", params = NULL)
{
  stopifnot(!missing(adjustmentSpec))

  # Define parameters
  reportBaseName <- tools::file_path_sans_ext(basename(adjustmentSpec$FileName))
  reportFileName <- ifelse(fileNameSuffix != "",
                           paste0(reportBaseName, "_", fileNameSuffix, ".Rmd"),
                           paste0(reportBaseName, ".Rmd"))
  reportFilePath <-
    system.file(file.path("reports", fileNameSuffix, reportFileName),
                package = "hivEstimatesAccuracy")

  report <- RenderReportToHTML(reportFilePath = reportFilePath,
                               params = list(InputData = params))

  return(report)
}
