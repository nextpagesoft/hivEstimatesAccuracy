#' RollingApply
#'
#' Applies a function to a centered rolling window of data.
#' This is a simplified version of function \code{rollaplly} from package \code{zoo}.
#'
#' @param data Vector of observations. Required.
#' @param width Integer specifying the window width (in numbers of observations). Required.
#' @param func Function to be applied. Required.
#' @param ... Additional arguments to be passed to \code{func}. Optional.
#'
#' @return NULL
#'
#' @examples
#' RollingApply(c(1, 2, 3, 4, 5), width = 3, func = sum)
#' RollingApply(c(1, 2, 3, 4, 5), width = 3, func = mean)
#' RollingApply(c(1, 2, 3, 4, 5), width = 3, func = sd)
#' RollingApply(c(1), width = 10, func = sd)
#' RollingApply(c(1), width = 10, func = mean)
#' RollingApply(c(), width = 10, func = sd)
#' RollingApply(c(), width = 10, func = mean)
#'
#' @export
RollingApply <- function(data, width, func, ...)
{
  stopifnot(!missing(data))
  stopifnot(!missing(width))
  stopifnot(!missing(func))

  if (length(data) == 0) {
    return(func(data))
  }

  width <- lapply(width, function(w) {
    seq(to = floor(w/2), length.out = w)
  })

  width <- if (length(width) == 1) {
    w <- rep(list(NULL), length(data))
    replace(w, seq(1, length(data), by = 1), width)
  }

  f <- function(i, data, ...) {
    offsets <- width[[i]]
    if (is.null(offsets)) {
      return(NULL)
    }
    posns <- i + offsets
    ix <- posns >= 1 & posns <= length(data)
    func(data[replace(posns, !ix, 0)], ...)
  }

  result <- sapply(seq_along(data), f, data)

  return(result)
}
