#' CreateOutputFileName
#'
#' Create output file name based on the input file name.
#'
#' @param inputFileName Input file name. Required.
#' @param outputPath Output path for the file. Optional.
#'   Default = \code{\link{dirname}(inputFileName)}.
#' @param prefix String to add at the beginning of the file name. Optional. Default = "".
#' @param suffix String to add at end beginning of the file name. Optional. Default = "".
#' @param extension File extension. Optional. Default = "txt".
#'
#' @return File name
#'
#' @examples
#' CreateOutputFileName(inputFileName = "test.xlsx")
#' CreateOutputFileName(inputFileName = "C:/Files/test.xlsx")
#' CreateOutputFileName(inputFileName = "C:/Files/test.xlsx", prefix = "Output_")
#'
#' @export
CreateOutputFileName <- function(inputFileName,
                                 outputPath = dirname(inputFileName),
                                 prefix = "",
                                 suffix = "",
                                 extension = "txt")
{
  stopifnot(!missing(inputFileName))

  nameSeed <- tools::file_path_sans_ext(basename(inputFileName))
  fileName <- sprintf("%s%s%s.%s", prefix, nameSeed, suffix, extension)

  outputFileName <- file.path(outputPath, fileName)

  return(outputFileName)
}
