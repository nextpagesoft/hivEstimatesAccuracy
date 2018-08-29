#' GetReportDefaultParams
#'
#' Gets default values of parameters defined in a report file
#'
#' @param params List of report parameter specifications
#' @param skipParamNames Names of parameters to filter out. Optional.
#'   Default = \code{NULL}
#'
#' @return Named list of values
#'
#' @examples
#' \dontrun{
#' GetReportDefaultParams(paramSpecs, skipParamNames)
#' }
#'
#' @export
GetReportDefaultParams <- function(params, skipParamNames = NULL)
{
  values <- sapply(params, "[[", "value")

  if (!is.null(skipParamNames)) {
    values <- values[!names(values) %in% skipParamNames]
  }

  values <- lapply(values, as.logical)

  return(values)
}
