#' RunApp
#'
#' Run the whole application. A Shiny application will start in the default web browser.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' RunApp()
#' }
#'
#' @export
RunApp <- function()
{
  appDir <- system.file("shiny", package = "hivEstimatesAccuracy")
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)

  return(invisible(NULL))
}
