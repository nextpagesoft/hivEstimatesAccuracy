#' RunApp
#'
#' Run the application.
#'
#' @param launchBrowser Logical indicating to open the app in a newly open web browser
#' @param appName Name of the application to run.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' RunApp()
#' }
#'
#' @export
RunApp <- function(launchBrowser = TRUE, appName = 'shiny')
{
  appDir <- system.file(appName, package = "hivEstimatesAccuracy")
  shiny::runApp(appDir, display.mode = "normal", launch.browser = launchBrowser)

  return(invisible(NULL))
}
