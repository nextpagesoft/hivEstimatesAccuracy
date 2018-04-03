#' ConvertToOrdinalNumber
#'
#' Convert from cardinal to ordinal number.
#'
#' @param cardinalNum Integer vector. Required.
#'
#' @return Character vector.
#'
#' @examples
#' ConvertToOrdinalNumber(cardinalNum = c(1, 2, 3, 11))
#'
#' @export
ConvertToOrdinalNumber <- function(cardinalNum)
{
  substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
  }

  tmp <- substrRight(as.character(cardinalNum), 2)

  mapping <- rbind(data.table(Cardinal = c("1", paste0(c(0, 2:9), 1)), Suffix = "st"),
                   data.table(Cardinal = c("2", paste0(c(0, 2:9), 2)), Suffix = "nd"),
                   data.table(Cardinal = c("3", paste0(c(0, 2:9), 3)), Suffix = "rd"))

  mapToSuffix <- match(tmp, mapping$Cardinal)
  suffix <- mapping$Suffix[mapToSuffix]
  suffix[is.na(suffix)] <- "th"

  ordinalNum <- paste0(cardinalNum, suffix)

  return(ordinalNum)
}
