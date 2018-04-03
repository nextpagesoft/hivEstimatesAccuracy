#' PlotMCMCDiagnostic
#'
#' Plot diagnostic information form MCMC chain convergence.
#'
#' @param plotData Array of matrices. Required.
#' @param plotType String denoting type of plot to create. Either "trace" for trace plot, or
#'   "autocorrelation" for autocorrelation plot. Default: "trace".
#' @param ... Extra parameters passed to \code{\link{plot}} if \code{plotType} is "trace" or to
#'   \code{\link[coda]{autocorr.plot}} if \code{plotType} is "autocorrelation".
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' PlotMCMCDiagnostic(plotData)
#' }
#'
#' @export
PlotMCMCDiagnostic <- function(plotData, plotType = "trace", ...)
{
  dataSize <- dim(plotData)
  nRows <- dataSize[1]
  nCols <- dataSize[2]
  nPoints <- dataSize[3]
  x <- seq_len(nPoints)

  if (plotType == "trace") {
    workFunc <- function(x, y) graphics::plot(x, y, type = "l", ylab = "", ...)
  } else {
    workFunc <- function(x, y) coda::autocorr.plot(y, auto.layout = FALSE, lag.max = nPoints, ...)
  }

  # Set layout (rows x cols) and margins
  graphics::par(mfrow = c(nRows, nCols),
      mar = c(1, 1, 1, 1))
  for (i in seq_len(nRows)) {
    for (j in seq_len(nCols)) {
      workFunc(x, plotData[i, j, x])
    }
  }

  invisible(NULL)
}
