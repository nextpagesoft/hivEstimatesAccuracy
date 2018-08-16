#' RenderReportToHTML
#'
#' Renders html fragment from RMarkdown document.
#'
#' @param reportFilePath Path to the source RMarkdown file. Required.
#' @param params Parameters passed to the report. Optional. Default = NULL.
#'
#' @return string with the rendered report
#'
#' @examples
#' \dontrun{
#' RenderReportToHTML(adjustmentSpec, fileNameSuffix, parameters)
#' }
#'
#' @export
RenderReportToHTML <- function(reportFilePath, params = NULL)
{
  stopifnot(!missing(reportFilePath))

  outDir <- tempfile()
  dir.create(outDir, recursive = TRUE)
  on.exit({
    unlink(outDir, recursive = TRUE)
  })

  htmlFileName <- RenderReportToFile(reportFilePath = reportFilePath,
                                     format = "html_fragment",
                                     params = params,
                                     outDir = outDir)
  reportFileContent <- ReadStringFromFile(htmlFileName)
  report <- HTML(reportFileContent)

  return(report)
}
