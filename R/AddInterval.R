AddInterval <- function(
  intervals,
  interval,
  idx = NULL,
  minYear = 1980L,
  maxYear = 2016L
) {
  if (!is.null(idx) && idx %between% c(1, nrow(intervals))) {
    intervals[idx, colnames(intervals) := interval]
    intervals <- copy(intervals)
  } else {
    intervals <- rbind(intervals, interval)
    intervals <- intervals[order(StartYear)]
  }

  intervals[1, StartYear := minYear]
  intervals[nrow(intervals), EndYear := maxYear]
  endYears <- intervals[seq_len(nrow(intervals) - 1) + 1, c(StartYear, maxYear)]
  intervals[, EndYear := endYears]

  return(intervals)
}
