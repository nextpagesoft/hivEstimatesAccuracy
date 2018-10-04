#' ReadRmdFrontMatter
#'
#' Reads YAML front matter from a Rmd file.
#'
#' @param filePath Path to the source RMarkdown file. Required.
#' @param section Name of section of the front matter to return. If not specified, then
#'   the whole specification is returned, otherwise only the specified section. Optional.
#'   Default = NULL.
#'
#' @return list object
#'
#' @examples
#' \dontrun{
#' ReadRmdFrontMatter(filePath)
#' }
#'
#' @export
ReadRmdFrontMatter <- function(filePath, section = NULL)
{
  stopifnot(!missing(filePath))

  # Read in the lines of file
  lines <- readLines(filePath)

  # Find the header portion contained between the --- lines.
  header_line_nums <- which(lines == "---") + c(1, -1)

  # Create a string of just that header portion
  header <- paste(lines[seq(header_line_nums[1],
                            header_line_nums[2])],
                  collapse = "\n")

  # Pparse it as yaml, which returns a list of property values
  frontMatter <- yaml::yaml.load(header)

  if (!is.null(section)) {
    frontMatter <- frontMatter[[section]]
  }

  return(frontMatter)
}
