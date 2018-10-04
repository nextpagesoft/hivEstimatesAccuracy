#' GetParamInfoFromAdjustSpec
#'
#' Extract parameters from an adjustment specification. Values are provided as
#' a list.
#'
#' @param paramSpecs List of parameter specifications. Required.
#' @param infoType Type of information to extract from the parameter
#'   specification. Optional. Default = "value".
#'
#' @return
#' List of parameters of shape \code{[paramName]} = \code{[paramValue]}
#'
#' @examples
#' paramSpecs <- list(
#'   nimp = list(
#'     label = "Number of imputations",
#'     input = "numeric",
#'     value = 2
#'   )
#' )
#' GetParamInfoFromAdjustSpec(paramSpecs)
#' # $nimp
#' # [1] 2
#'
#' GetParamInfoFromAdjustSpec(paramSpecs, infoType = "label")
#' GetParamInfoFromAdjustSpec(paramSpecs, infoType = "input")
#'
#' @export
GetParamInfoFromAdjustSpec <- function(paramSpecs, infoType = "value")
{
  # Check if required inputs are provided
  stopifnot(!missing(paramSpecs))

  if (is.null(paramSpecs)) {
    return(list())
  }

  # Extract parameters
  params <- lapply(paramSpecs, "[[", infoType)

  return(params)
}
