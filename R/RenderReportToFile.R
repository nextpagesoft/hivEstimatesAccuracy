#' RenderReportToFile
#'
#' Creates a report from a specified RMarkdown file.
#'
#' @param filePath Path to the source RMarkdown file. Required.
#' @param format Output format of the report. Optional. Default = \code{"html_fragment"}.
#' @param ... Additional arguments passed to \link[rmarkdown]{render}. Optional.
#'
#' @return File name of the generated report
#'
#' @seealso \pkg{rmarkdown}
#'
#' @examples
#' \dontrun{
#' RenderReportToFile(
#'   filePath,
#'   params = list(AdjustedData = data.table::data.table(A = c(1, 2), B = c(3, 4))))
#' }
#'
#' @export
RenderReportToFile <- function(filePath, format = "html_fragment", ...)
{
  stopifnot(!missing(filePath))

  reportFileName <- rmarkdown::render(input = filePath,
                                      output_format = format,
                                      runtime = "static",
                                      run_pandoc = TRUE,
                                      clean = TRUE,
                                      quiet = TRUE,
                                      ...)

  return(reportFileName)
}
