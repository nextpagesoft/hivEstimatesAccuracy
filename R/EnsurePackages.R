#' EnsurePackages
#'
#' Load packages. They will be installed if needed.
#'
#' @param packageNames Vector of package names to load. Required.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' EnsurePackages(packageNames = c("jomo"))
#' }
#'
#' @export
EnsurePackages <- function(packageNames)
{
  # Check if required inputs are provided
  stopifnot(!missing(packageNames))

  for (packageName in packageNames) {
    # Try loading the package
    if (!requireNamespace(packageName, character.only = TRUE, quietly = TRUE)) {
      # Package is not available. Install it first.
      utils::install.packages(packageName)
      requireNamespace(packageName, character.only = TRUE, quietly = TRUE)
    }
  }

  return(invisible(NULL))
}
