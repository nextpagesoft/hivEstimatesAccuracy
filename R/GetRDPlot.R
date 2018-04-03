#' GetRDPlots
#'
#' Get plots for reporting delay adjustment artifacts.
#'
#' @param stratum String denoting for which value of Stratum column in \code{plotData} to make plots.
#'   If not specifed, then plots are created for total. Optional. Default = \code{NULL}.
#' @param plotData Data table object. Required.
#' @param isOriginalData Logical indicating that \code{plotData} is based on data before any other
#'   adjustment. Optional. Default = TRUE.
#' @param colorPalette Character vector of length 4 with colors for plotted series. Optional.
#'   Default = \code{c("#AAAAAA", "#E69F00", "#000000", "#56B4E9")}.
#'
#' @return Data table object
#'
#' @examples
#' \dontrun{
#' GetRDPlots(stratum, plotData, isOriginalData, colorPalette)
#' }
#'
#' @export
GetRDPlots <- function(
  stratum = NULL,
  plotData,
  isOriginalData = TRUE,
  colorPalette = c("#AAAAAA", "#E69F00", "#000000", "#56B4E9"))
{
  if (!is.null(stratum)) {
    localPlotData <- plotData[Stratum == stratum]
  } else {
    localPlotData <- plotData
  }

  localConfBoundsPlotData <- localPlotData[Source == ifelse(isOriginalData,
                                                            "Reported",
                                                            "Imputed")]
  plot <-
    ggplot(data = localPlotData, aes(x = DateOfDiagnosisYear)) +
    geom_ribbon(data = localConfBoundsPlotData,
                aes(ymin = pmin(pmax(LowerEstCount, 0), max(EstCount)),
                    ymax = pmin(pmax(UpperEstCount, 0), max(EstCount)),
                    fill = "95% confidence interval\nfor estimated total count"),
                alpha = 0.4) +
    scale_fill_manual("Bounds", values = colorPalette[1]) +
    geom_line(data = localConfBoundsPlotData,
              aes(x = DateOfDiagnosisYear, y = EstCount, color = "Estimated total"), size = 0.5) +
    geom_line(aes(y = Count, group = Source, color = Source), size = 0.2) +
    scale_colour_manual("Counts", values = colorPalette[2:4]) +
    xlab("Diagnosis year") +
    ylab("Count of HIV cases")

  if (!is.null(stratum)) {
    plot <- plot +
      ggtitle(paste("Reported and estimated total count of cases, by", stratum)) +
      facet_wrap(~ StratumValue, ncol = 2)
  } else {
    plot <- plot +
      ggtitle("Reported and estimated total count of cases")
  }

  return(plot)
}
