#' GetIsState
#'
#' Indicates that the input file is a state file
#'
#' @param content object. Required.
#'
#' @return logical
#'
#' @examples
#' \dontrun{
#' GetIsState(content = "A")
#' }
#'
#' @export
GetIsState <- function(content)
{
  valid <- is.list(content) &&
    all(c("OriginalData", "InputData", "AdjustedData") %in% names(content))

  return(valid)
}
