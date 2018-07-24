#' GetMiceDiagnosticPlot
#'
#' Gets mice diagnostic plot.
#'
#' @param mids Object of class "mids" (output of \code{\link[mice]{mice}}) function.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' GetMiceDiagnosticPlot(mids)
#' }
#'
#' @export
GetMiceDiagnosticPlot <- function(mids)
{
  colors <- c("#69b023", "#7bbcc0", "#c44f27", "#ad2251", "#5d2083")

  mn <- mids$chainMean
  sm <- sqrt(mids$chainVar)

  # Keep only maximum 5 first imputations
  mn <- mn[, , seq(min(5L, dim(mn)[3]))]
  sm <- sm[, , seq(min(5L, dim(sm)[3]))]

  meanData <- as.data.table(mn)
  setnames(meanData, c("Row", "Sample", "Imputation", "Value"))
  ConvertDataTableColumns(meanData, c(Sample = "integer"))
  meanData[, Column := "Mean"]

  smData <- as.data.table(sm)
  setnames(smData, c("Row", "Sample", "Imputation", "Value"))
  ConvertDataTableColumns(smData, c(Sample = "integer"))
  smData[, Column := "Standard deviation"]

  plotData <- rbind(meanData, smData)
  plotData[, Element := paste(Row, Column, sep = " - ")]
  setorderv(plotData, c("Row", "Column", "Sample", "Imputation"))

  dims <- plotData[, c(length(unique(Row)), length(unique(Column)))]

  micePlot <- ggplot(plotData, aes(x = Sample)) +
    geom_line(aes(y = Value, colour = Imputation)) +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_colour_manual(values = colors) +
    ylab("Coefficients") +
    facet_wrap(~Element,
               scales = "free_y",
               strip.position = "top",
               nrow = dims[1],
               ncol = dims[2]) +
    theme(strip.background = element_rect(fill = "#e9e9e9",
                                          linetype = "blank"),
          strip.placement = "outside",
          strip.text = element_text(size = 8),
          plot.title = element_text(size = 13),
          axis.title.x =  element_text(size = 10),
          axis.title.y =  element_text(size = 10),
          axis.text = element_text(size = 8),
          text = element_text(size = 11),
          strip.switch.pad.wrap = unit(0, "points"),
          panel.border = element_rect(colour = "#888888"),
          axis.line = element_line(colour = "#888888"),
          axis.ticks = element_line(colour = "#888888"))

  invisible(micePlot)
}
