#' RunApp
#'
#' Run the application.
#'
#' @param launchBrowser Logical indicating to open the app in a newly open web browser
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' RunApp()
#' }
#'
#' @export
RunApp <- function(launchBrowser = TRUE)
{
  appDir <- system.file("shiny", package = "hivEstimatesAccuracy")
  shiny::runApp(appDir, display.mode = "normal", launch.browser = launchBrowser)

  return(invisible(NULL))
}
