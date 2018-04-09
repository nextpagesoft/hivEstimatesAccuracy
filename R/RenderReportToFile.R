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

  tempReportFilePath <- file.path(tempdir(), "reports", basename(filePath))

  dir.create(dirname(tempReportFilePath), showWarnings = FALSE, recursive = TRUE)

  file.copy(filePath, tempReportFilePath, overwrite = TRUE)
  on.exit({
    unlink(tempReportFilePath)
  })

  if ("word_document" %in% format) {
    dir.create(file.path(dirname(tempReportFilePath), "resources"),
               showWarnings = FALSE, recursive = TRUE)

    file.copy(system.file("reports/resources/template_ECDC.docx", package = "hivEstimatesAccuracy"),
              file.path(dirname(tempReportFilePath), "resources/template_ECDC.docx"))

    on.exit({
      unlink(tempReportFilePath)
      unlink(file.path(dirname(tempReportFilePath), "resources"),
             recursive = TRUE)
    })

  }

  reportFileName <- rmarkdown::render(input = tempReportFilePath,
                                      output_format = format,
                                      runtime = "static",
                                      run_pandoc = TRUE,
                                      clean = TRUE,
                                      quiet = TRUE,
                                      envir = new.env(parent = globalenv()),
                                      ...)

  return(reportFileName)
}
