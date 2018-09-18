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
#'   Default = \code{c("#c7c7c7", "#69b023", "#7bbcc0", "#9d8b56", "#ce80ce")}.
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
  colorPalette = c("Bounds" = "#c7c7c7",
                   "Estimated total" = "#69b023",
                   "Reported" = "#7bbcc0",
                   "Imputed" = "#9d8b56")
)
{
  if (!is.null(stratum)) {
    localPlotData <- plotData[Stratum == stratum]
    localPlotData[, StratumValue := factor(StratumValue)]
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
    scale_fill_manual("Bounds", values = colorPalette[["Bounds"]]) +
    geom_line(data = localConfBoundsPlotData,
              aes(x = DateOfDiagnosisYear, y = EstCount, color = "Estimated total"), size = 1) +
    geom_line(aes(y = Count, group = Source, color = Source), size = 1) +
    scale_colour_manual("Counts", values = colorPalette) +
    scale_x_continuous(expand = c(0, 0), breaks = localPlotData[, sort(unique(DateOfDiagnosisYear))]) +
    scale_y_continuous(expand = c(0, 0)) +
    expand_limits(y = 0) +
    theme_classic() +
    theme(plot.title = element_text(size = 11),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          text = element_text(size = 11),
          panel.grid = element_blank(),
          panel.spacing = unit(2, "lines"),
          axis.line = element_line(colour = "#888888"),
          axis.ticks = element_line(colour = "#888888"),
          strip.background = element_rect(fill = "#e9e9e9",
                                          linetype = "blank"),
          strip.placement = "outside",
          strip.text = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    xlab("Diagnosis year") +
    ylab("Count of HIV cases")

  if (!is.null(stratum)) {
    plot <-
      plot +
      ggtitle(paste("Reported and estimated total count of cases by", stratum)) +
      facet_wrap(~StratumValue, ncol = 2, drop = FALSE, shrink = FALSE)
  } else {
    plot <- plot +
      ggtitle("Reported and estimated total count of cases")
  }

  return(plot)
}
