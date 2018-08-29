#' RenderReportToFile
#'
#' Creates a report from a specified RMarkdown file.
#'
#' @param reportFilePath Path to the source RMarkdown file. Required.
#' @param format Output format of the report. Optional. Default = \code{"html_fragment"}.
#' @param outputFilePath Path to the output file. Optional. Default = \code{NULL}.
#' @param outDir Output directory. Temporary directory is used if \code{NULL}. Optional.
#'   Default = \code{\link{dirname}(reportFilePath)}.
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
RenderReportToFile <- function(
  reportFilePath,
  format = "html_fragment",
  outputFilePath = NULL,
  outDir = dirname(reportFilePath),
  ...)
{
  stopifnot(!missing(reportFilePath))
  stopifnot(!is.null(outputFilePath) || !is.null(outDir))
  stopifnot(length(format) == 1)

  tempReportFilePath <- file.path(tempdir(),
                                  "report",
                                  basename(reportFilePath))
  reportDirName <- dirname(tempReportFilePath)

  dir.create(reportDirName, showWarnings = FALSE, recursive = TRUE)
  file.copy(reportFilePath,
            tempReportFilePath,
            overwrite = TRUE)
  on.exit({
    unlink(reportDirName, recursive = TRUE)
  })

  if (format == "all" && "word_document" %in% format) {
    dir.create(file.path(reportDirName, "resources"),
               showWarnings = FALSE, recursive = TRUE)

    file.copy(system.file("reports/resources/template_ECDC.docx",
                          package = "hivEstimatesAccuracy"),
              file.path(reportDirName, "resources/template_ECDC.docx"))
  }

  tempOutputFilePath <- rmarkdown::render(input = tempReportFilePath,
                                          output_format = format,
                                          runtime = "static",
                                          run_pandoc = TRUE,
                                          clean = TRUE,
                                          quiet = TRUE,
                                          envir = new.env(parent = globalenv()),
                                          ...)

  if (format == "latex_document") {
    cachedWD <- getwd()
    setwd(reportDirName)
    on.exit({
      setwd(cachedWD)
    }, add = TRUE, after = FALSE)

    unlink(tempReportFilePath)
    reportFileNames <- dir(path = reportDirName,
                           pattern = ".",
                           recursive = TRUE,
                           full.names = FALSE,
                           include.dirs = FALSE)

    tempOutputFilePath <- paste0(tools::file_path_sans_ext(tempOutputFilePath), ".zip")
    zip(tempOutputFilePath,
        files = reportFileNames)

  }

  if (is.null(outputFilePath)) {
    outputFilePath <- file.path(outDir, basename(tempOutputFilePath))
  }
  file.copy(tempOutputFilePath,
            outputFilePath,
            overwrite = TRUE)

  return(outputFilePath)
}
