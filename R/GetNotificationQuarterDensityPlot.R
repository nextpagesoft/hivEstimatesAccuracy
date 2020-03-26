#' GetNotificationQuarterDensityPlot
#'
#' Get density plot of notification quarter
#'
#' @param plotData Data table object. Required.
#' @param colorPalette Character vector of colors for plotted
#'  series. Optional. Default = \code{c("#c7c7c7", "#69b023", "#7bbcc0",
#'  "#9d8b56", "#ce80ce")}.
#' @param genderLabels Named vector of mapping from Gender code to label.
#'   Optional. Default = \code{c("M" = "Male", "F" = "Female", "O" = "Other")}
#' @param xLimits Vector of two numbers (years), lower and upper limit for the
#'   x axis. Optional. Default = \code{NULL}.
#' @param markerLocations Vector of numbers (years) for positions of vertical
#'   dashed lines. Optional. Default = \code{NULL}.
#'
#' @return ggplot2 object
#'
#' @examples
#' \dontrun{
#' GetNotificationQuarterDensityPlot(plotData)
#' }
#'
#' @export
GetNotificationQuarterDensityPlot <- function(
  plotData,
  colorPalette = c("#69b023", "#7bbcc0", "#9d8b56", "#ce80ce"),
  genderLabels = c("M" = "Male", "F" = "Female", "O" = "Other"),
  xLimits = NULL,
  markerLocations = NULL
)
{
  if (is.null(plotData)) {
    return(NULL)
  }

  plotDt <- plotData[, .(Count = .N), by = .(NotificationTime, Gender)]
  if (!is.null(markerLocations)) {
    plotDt[, Selected := NotificationTime %between% markerLocations]
  } else {
    plotDt[, Selected := TRUE]
  }

  if (is.null(xLimits)) {
    xLimits <- plotDt[, c(floor(min(NotificationTime, na.rm = TRUE)),
                          ceiling(max(NotificationTime, na.rm = TRUE)))]
  }

  breaks <- seq(from = xLimits[1],
                to = xLimits[2],
                by = 1)
  labels <- as.character(breaks)
  labels[breaks %% 2 != 0] <- ""

  plot <-
    ggplot(plotDt, aes(x = factor(NotificationTime), y = Count, fill = Gender, color = Gender)) +
    geom_col(aes(alpha = Selected), position = "stack", width = 1, size = 0.1) +
    scale_colour_manual("Gender",
                        values = colorPalette,
                        labels = genderLabels) +
    scale_fill_manual("Gender",
                      values = colorPalette,
                      labels = genderLabels) +
    scale_alpha_manual(values = c("TRUE" = 0.7, "FALSE" = 0.2), guide = "none") +
    scale_x_discrete(expand = c(0, 0), breaks = breaks, labels = labels) +
    scale_y_continuous(expand = c(0, 0)) +
    # expand_limits(x = xLimits, y = 0) +
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
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9, angle = 90, hjust = 0.5)) +
    xlab("Notification quarter") +
    ylab("Count of cases")

  return(plot)
}
