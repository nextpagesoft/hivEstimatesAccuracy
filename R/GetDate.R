#' GetDate
#'
#' @param y y
#' @param q q
#' @param m m
#' @param w w
#' @param d d
#'
#' @return Date
#'
#' @examples
#' GetDate(1977, 2, 5, NA, 20)
#'
#' @export
GetDate <- function(
  y = NA,
  q = NA,
  m = NA,
  w = NA,
  d = NA
) {
  temp_d <- d
  temp_w <- w
  temp_W <- paste0(gsub('-', '', w), '1')
  temp_m <- m
  temp_q <- q
  temp_y <- y
  temp_Date <- as.Date(temp_W, '%Y%W%u')

  # Day and month missing but week observed
  temp_d <- ifelse(
    is.na(temp_d) & is.na(temp_m) & !(is.na(temp_w) | temp_w == ''),
    as.numeric(strftime(temp_Date, '%d')),
    temp_d
  )
  temp_m <- ifelse(
    is.na(temp_m) & !(is.na(temp_w) | temp_w == ''),
    as.numeric(strftime(temp_Date, '%m')),
    temp_m
  )

  # Day, week, month missing but quarter observed
  # Q1
  temp_d <- ifelse(is.na(temp_m) & temp_q == 1, 14, temp_d)
  temp_m <- ifelse(is.na(temp_m) & temp_q == 1, 2, temp_m)
  # Q2
  temp_d <- ifelse(is.na(temp_m) & temp_q == 2, 16, temp_d)
  temp_m <- ifelse(is.na(temp_m) & temp_q == 2, 5, temp_m)
  # Q3
  temp_d <- ifelse(is.na(temp_m) & temp_q == 3, 15, temp_d)
  temp_m <- ifelse(is.na(temp_m) & temp_q == 3, 8, temp_m)
  # Q4
  temp_d <- ifelse(is.na(temp_m) & temp_q == 4, 15, temp_d)
  temp_m <- ifelse(is.na(temp_m) & temp_q == 4, 11, temp_m)

  # Day missing, not missing month
  temp_d <- ifelse(is.na(temp_d) & !is.na(temp_m), 15, temp_d)

  # Day and month missing
  temp_m <- ifelse(is.na(temp_d) & is.na(temp_m), 7, temp_m)
  temp_d <- ifelse(is.na(temp_d), 1, temp_d)

  temp_date <- as.Date(ISOdate(temp_y, temp_m, temp_d))
  return(temp_date)
}
