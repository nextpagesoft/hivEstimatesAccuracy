#' GetMCMCAutoCorrelationPlot
#'
#' Get autocorrelation plot from MCMC chain convergence.
#'
#' @param covariance Array of covariance matrices. Required.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' GetMCMCAutoCorrelationPlot(covariance)
#' }
#'
#' @export
GetMCMCAutoCorrelationPlot <- function(covariance)
{
  dims <- dim(covariance)

  plotData <- data.table::setDT(as.data.frame.table(covariance,
                                                    responseName = "Value"))
  setnames(plotData, c("Row", "Column", "Sample", "Covariance"))
  plotData[, ":="(
    Sample = as.integer(Sample)
  )]

  setorderv(plotData, c("Row", "Column", "Sample"))
  plotData[, AutoCorrelation := acf(Covariance, lag.max = dims[3], plot = FALSE)$acf,
           by = .(Row, Column)]

  acPlot <- ggplot(plotData, aes(x = Sample)) +
    geom_segment(aes(xend = Sample, yend = 0, y = AutoCorrelation),
                 colour = "#69b023",
                 na.rm = TRUE) +
    geom_hline(aes(yintercept = 0),
               colour = "#888888") +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    ylab("Autocorrelation") +
    facet_grid(Row ~ Column) +
    theme(strip.background = element_rect(fill = "#e9e9e9",
                                          linetype = "blank"),
          strip.placement = "outside",
          strip.text = element_text(size = 8),
          plot.title = element_text(size = 13),
          axis.title.x =  element_text(size = 10),
          axis.title.y =  element_text(size = 10),
          axis.text = element_text(size = 8),
          text = element_text(size = 11),
          panel.border = element_rect(colour = "#888888"),
          axis.line = element_line(colour = "#888888"),
          axis.ticks = element_line(colour = "#888888"))

  invisible(acPlot)
}
