#' GetMCMCBetasPlot
#'
#' Get plot of Betas from MCMC chain convergence.
#'
#' @param betas Array of beta matrices. Required.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' GetMCMCBetasPlot(betas)
#' }
#'
#' @export
GetMCMCBetasPlot <- function(betas)
{
  dims <- dim(betas)

  plotData <- data.table::setDT(as.data.frame.table(betas, responseName = "Value"))
  setnames(plotData, c("Row", "Column", "Sample", "Beta"))

  plotData[, ":="(
    Row = as.character(Row),
    Column = as.character(Column),
    Sample = as.integer(Sample),
    Element = paste(Row, Column, sep = " - ")
  )]
  setorderv(plotData, c("Column", "Row"))

  betasPlot <- ggplot(plotData, aes(x = Sample)) +
    geom_line(aes(y = Beta), colour = "#69b023") +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    ylab("Beta coefficients") +
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

  invisible(betasPlot)
}
