#' GetDataSummaryArtifacts
#'
#' Produces plots and artifacts for the data summary page
#'
#' @param inputData Input data as data.table. Required
#'
#' @return list of objects
#'
#' @examples
#' \dontrun{
#' GetDataSummaryArtifacts(inputData)
#' }
#'
#' @export
GetDataSummaryArtifacts <- function(inputData)
{
  if (is.null(inputData)) {
    return(list(MissPlot = NULL,
                DelayDensFullPlot = NULL,
                DelayDensShortPlot = NULL,
                MeanDelayPlot = NULL))
  }

  colors <- c("#69b023", "#d9d9d9", "#7bbcc0")

  inputData <- copy(inputData)

  missPlotsTotal <- GetMissingnessPlots(inputData)
  inputDataGender <- split(inputData, by = "Gender")
  if (length(inputDataGender) > 0) {
    missPlotsByGender <- lapply(inputDataGender, GetMissingnessPlots)
  } else {
    missPlotsByGender <- NULL
  }

  # New stuff
  inputData[, ":="(
    NotificationTime = DateOfNotificationYear + 1/4 * DateOfNotificationQuarter,
    DiagnosisTime = DateOfDiagnosisYear + 1/4 * DateOfDiagnosisQuarter
  )]
  inputData[, VarX := 4 * (NotificationTime - DiagnosisTime)]
  inputData[VarX < 0, VarX := NA]

  # Observed delay distribution
  if (!all(is.na(inputData$VarX))) {
    quant95 <- quantile(inputData$VarX, probs = 0.95, na.rm = TRUE)
    quant99 <- quantile(inputData$VarX, probs = 0.99, na.rm = TRUE)
    delayDensFullPlot <- ggplot(data = inputData, mapping = aes(x = VarX)) +
      geom_density(fill = colors[2], alpha = 0.6, adjust = 4, size = 1, colour = colors[1]) +
      geom_vline(xintercept = quant95, linetype = "dashed") +
      annotate("text",
               label = paste("95% of cases reported \nby", prettyNum(quant95), "quarters"),
               fontface = "italic",
               x = quant95 + 0.5,
               y = -Inf,
               hjust = 0,
               vjust = -1) +
      expand_limits(x = c(0, max(30, ceiling(quant99)))) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic() +
      theme(text = element_text(size = 14),
            panel.grid = element_blank(),
            axis.line = element_line(colour = "#888888"),
            axis.ticks = element_line(colour = "#888888")) +
      ggtitle("Density of Reporting Delay\n") +
      xlab("Delay in quarters of the year") +
      ylab("Proportion reported with the delay")

    shortData <- inputData[DateOfDiagnosisYear < max(DateOfDiagnosisYear) - 5]
    quant95 <- quantile(shortData$VarX, probs = 0.95, na.rm = TRUE)
    quant99 <- quantile(shortData$VarX, probs = 0.99, na.rm = TRUE)
    delayDensShortPlot <- ggplot(data = shortData, mapping = aes(x = VarX)) +
      geom_density(fill = colors[2], alpha = 0.6, adjust = 4, size = 1, colour = colors[1]) +
      geom_vline(xintercept = quant95, linetype = "dashed") +
      annotate("text",
               label = paste("95% of cases reported \nby", prettyNum(quant95), "quarters"),
               fontface = "italic",
               x = quant95 + 0.5,
               y = -Inf,
               hjust = 0,
               vjust = -1) +
      expand_limits(x = c(0, max(30, ceiling(quant99)))) +
      scale_x_continuous(expand = c(0, 0)) +
      theme_classic() +
      theme(text = element_text(size = 14),
            panel.grid = element_blank(),
            axis.line = element_line(colour = "#888888"),
            axis.ticks = element_line(colour = "#888888")) +
      ggtitle("Density of Reporting Delay with last 5 years deleted") +
      xlab("Delay in quarters of the year") +
      ylab("Proportion reported with the delay")
    if (nrow(shortData) > 0) {
      delayDensShortPlot <- delayDensShortPlot +
        scale_y_continuous(expand = c(0, 0))
    }
  } else {
    delayDensFullPlot <- NULL
    delayDensShortPlot <- NULL
  }

  # Mean delay plot
  meanDelay <- inputData[!is.na(NotificationTime),
                         .(MeanDelay = mean(VarX, na.rm = TRUE)),
                         by = .(NotificationTime)]

  if (nrow(meanDelay) > 0) {
    meanDelay[, rsd := RollingApply(MeanDelay, 10, sd)]
    meanDelayCurve <- meanDelay[, as.data.table(lowess(NotificationTime, MeanDelay))]
    meanDelayUpper <- meanDelay[, as.data.table(lowess(NotificationTime, MeanDelay + 1.96 * rsd))]
    meanDelayPlot <- ggplot() +
      geom_point(data = meanDelay, aes(x = NotificationTime, y = MeanDelay), alpha = 0.5) +
      geom_line(data = meanDelayCurve, aes(x = x, y = y,
                                           color = "Smoothed mean reporting delay",
                                           linetype = "Smoothed mean reporting delay"), size = 1) +
      geom_line(data = meanDelayUpper, aes(x = x, y = y,
                                           color = "Expected upper bound",
                                           linetype = "Expected upper bound"), size = 1) +
      labs(colour = "Datasets", x = "xxx", y = "yyy") +
      scale_color_manual("Datasets", values = c(colors[3], colors[1])) +
      scale_linetype_manual("Datasets",
                            values = c("Smoothed mean reporting delay" = "solid",
                                       "Expected upper bound" = "dotted")) +
      scale_x_continuous(expand = c(0, 0), breaks = meanDelay[, sort(unique(round(NotificationTime)))]) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic() +
      theme(text = element_text(size = 14),
            panel.grid = element_blank(),
            axis.line = element_line(colour = "#888888"),
            axis.ticks = element_line(colour = "#888888"),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11)) +
      ggtitle("Reporting delay by notification quarter") +
      xlab("Notification time in quarters of the year") +
      ylab("Mean reporting delay [quarter]")

  } else {
    meanDelayPlot <- NULL
  }

  return(list(MissPlotsTotal = missPlotsTotal,
              MissPlotsByGender = missPlotsByGender,
              DelayDensFullPlot = delayDensFullPlot,
              DelayDensShortPlot = delayDensShortPlot,
              MeanDelayPlot = meanDelayPlot))
}
