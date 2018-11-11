#' MakeNames
#'
#' Create a unique vector of names based on strings.
#'
#' @param strings Vector of strings. Required.
#'
#' @return (Named) vector of unique, valid names.
#'
#' @examples
#' strings <- c(adjustment1 = "Adjustment 1", adjustment2  = "Fake adjustment")
#' MakeNames(strings)
#'
#' strings <- c("Adjustment 1", "Fake adjustment")
#' MakeNames(strings)
#'
#' @export
MakeNames <- function(strings)
{
  names <- setNames(gsub("\\.", "_", make.names(tolower(strings), unique = TRUE)),
                    names(strings))
  return(names)
}
