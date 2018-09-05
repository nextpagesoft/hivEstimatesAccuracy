#' RecordGgplot
#'
#' Description
#'
#' @param plotObj ggplot object
#'
#' @return recorded plot object
#'
#' @examples
#' \dontrun{
#' RecordGgplot(plotObj)
#' }
#'
#' @export
RecordGgplot <- function(plotObj)
{
  pdf(NULL)
  dev.control(displaylist = "enable")
  print(plotObj)
  pl <- recordPlot()
  invisible(dev.off())

  return(pl)
}
