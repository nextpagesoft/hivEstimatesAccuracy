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

  colors <- c("#69b023", "#9d8b56", "#7bbcc0", "#ce80ce")
  fillColor <- "#d9d9d9"

  inputData <- copy(inputData)

  # Proportion of missing
  missPropData <- inputData[, .(
    Transmission = sum(is.na(Transmission)) / .N,
    Age = sum(is.na(Age)) / .N,
    CD4 = sum(is.na(SqCD4)) / .N,
    GroupedRegionOfOrigin = sum(is.na(GroupedRegionOfOrigin)) / .N
  ), by = .(DateOfDiagnosisYear)]
  missPropData <- melt(missPropData,
               id.vars = c("DateOfDiagnosisYear"),
               variable.name = "Data",
               variable.factor = TRUE,
               value.name = "Proportion")
  breaks <- missPropData[, sort(unique(DateOfDiagnosisYear))]
  labels <- as.character(breaks)
  missPropPlot <- ggplot(data = missPropData) +
    geom_point(
      mapping = aes(
        x = DateOfDiagnosisYear,
        y = Proportion,
        color = Data,
        group = Data
      ),
      position = 'identity',
      size = 2) +
    geom_line(
      mapping = aes(
        x = DateOfDiagnosisYear,
        y = Proportion,
        color = Data,
        group = Data
      ),
      linetype = 'dotted',
      position = 'identity',
      size = 0.5) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = breaks,
                       labels = labels) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_manual(name = "Data",
                       values = colors) +
    ggtitle("Proportion of missing values") +
    xlab("Diagnosis year") +
    theme_classic() +
    theme(plot.title = element_text(size = 12, face = "plain"),
          text = element_text(size = 12, face = "plain"),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "#888888"),
          axis.ticks = element_line(colour = "#888888"))

  # Generic missingness plots
  missPlotsTotal <- GetMissingnessPlots(inputData)
  inputDataGender <- split(inputData, by = "Gender")
  if (length(inputDataGender) > 0) {
    missPlotsByGender <- lapply(inputDataGender, GetMissingnessPlots)
  } else {
    missPlotsByGender <- NULL
  }

  # Reporting delays missingness plots
  missPlotsRD <-
    GetMissingnessPlots(
      inputData,
      columnNames = c("DateOfDiagnosisYear", "DateOfDiagnosisQuarter",
                      "DateOfNotificationYear", "DateOfNotificationQuarter"),
      labels = c("Diagnosis year", "Diagnosis quarter", "Notification year",
                 "Notification quarter"))

  # Reporting delay density plot
  densDelay <- inputData[!is.na(VarX)][VarX >= 0]
  if (densDelay[, .N > 0]) {
    quant95 <- quantile(densDelay$VarX, probs = 0.95, na.rm = TRUE)
    quant99 <- quantile(densDelay$VarX, probs = 0.99, na.rm = TRUE)
    delayDensFullPlot <- ggplot(data = densDelay, mapping = aes(x = VarX)) +
      geom_density(fill = fillColor, alpha = 0.6, adjust = 4, size = 1, colour = colors[1]) +
      geom_vline(xintercept = quant95, linetype = "dashed") +
      annotate("text",
               label = paste("95% of cases reported \nby", prettyNum(quant95), "quarters"),
               fontface = "italic",
               x = quant95 + 0.5,
               y = -Inf,
               hjust = 0,
               vjust = -1) +
      scale_x_continuous(expand = c(0, 0), breaks = sort(unique(densDelay$VarX))) +
      scale_y_continuous(expand = c(0, 0)) +
      coord_cartesian(xlim = c(0, max(20, ceiling(quant99)))) +
      theme_classic() +
      theme(plot.title = element_text(size = 12, face = "plain"),
            text = element_text(size = 12, face = "plain"),
            panel.grid = element_blank(),
            axis.line = element_line(colour = "#888888"),
            axis.ticks = element_line(colour = "#888888")) +
      ggtitle("Density of Reporting Delay\n") +
      xlab("Delay in quarters of the year") +
      ylab("Proportion reported with the delay")
  } else {
    delayDensFullPlot <- NULL
  }

  # Mean delay plot
  meanDelay <- inputData[!is.na(NotificationTime),
                         .(MeanDelay = mean(VarX, na.rm = TRUE)),
                         by = .(NotificationTime)]

  if (meanDelay[, .N > 0]) {
    breaks <- meanDelay[, sort(unique(round(NotificationTime)))]
    labels <- as.character(breaks)

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
      scale_x_continuous(expand = c(0, 0),
                         breaks = breaks,
                         labels = labels) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic() +
      theme(plot.title = element_text(size = 12, face = "plain"),
            text = element_text(size = 12, face = "plain"),
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
              MissPlotsRD = missPlotsRD,
              DelayDensFullPlot = delayDensFullPlot,
              MeanDelayPlot = meanDelayPlot,
              MissPropPlot = missPropPlot))
}
