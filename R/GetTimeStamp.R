#' GetTimeStamp
#'
#' Get time stamp of the form "YYYYmmddHHMMSS" in a given time zone. By default the current time
#' zone is used.
#'
#' @param timeZone Time zone to use when getting current time. Optional.
#'   Default = \code{\link{Sys.timezone}()}.
#'
#' @return Timestamp string.
#'
#' @examples
#' GetTimeStamp()
#' GetTimeStamp(timeZone = "GMT")
#'
#' @export
GetTimeStamp <- function(timeZone = Sys.timezone())
{
  timeStamp <- format(Sys.time(), "%Y%m%d%H%M%S", tz = timeZone)

  return(timeStamp)
}
