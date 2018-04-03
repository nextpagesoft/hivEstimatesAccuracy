#' RenderReportToHTML
#'
#' Renders html fragment from RMarkdown document.
#'
#' @param filePath Path to the source RMarkdown file. Required.
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
RenderReportToHTML <- function(filePath, params = NULL)
{
  stopifnot(!missing(filePath))

  htmlFileName <- RenderReportToFile(filePath = filePath,
                                     format = "html_fragment",
                                     params = params)

  on.exit({
    unlink(htmlFileName)
  })

  reportFileContent <- ReadStringFromFile(htmlFileName)
  report <- HTML(reportFileContent)

  return(report)
}
