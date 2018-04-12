library(data.table)
library(ggplot2)

dims <- dim(adjustedData[[i]]$Artifacts$F$Beta)

plotData <- data.table::setDT(as.data.frame.table(adjustedData[[i]]$Artifacts$F$Beta,
                                                  responseName = "Value"))
setnames(plotData, c("Row", "Column", "Sample", "Beta"))
plotData[, ":="(
  Sample = as.integer(Sample),
  Element = paste(Row, Column, sep = " / ")
)]

# Betas

ggplot(plotData, aes(x = Sample)) +
  geom_line(aes(y = Beta), colour = "#69b023") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Betas") +
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


print(GetMCMCBetasPlot(adjustedData[[i]]$Artifacts$F$Beta))
print(GetMCMCBetasPlot(adjustedData[[i]]$Artifacts$M$Beta))
print(GetMCMCAutoCorrelationPlot(adjustedData[[i]]$Artifacts$F$Covariance))
print(GetMCMCAutoCorrelationPlot(adjustedData[[i]]$Artifacts$M$Covariance))


# Autocorrelation

plotData <- data.table::setDT(as.data.frame.table(adjustedData[[i]]$Artifacts$F$Covariance,
                                                  responseName = "Value"))
setnames(plotData, c("Row", "Column", "Sample", "Covariance"))
plotData[, ":="(
  Sample = as.integer(Sample)
)]

setorderv(plotData, c("Row", "Column", "Sample"))
plotData[, AutoCorrelation := acf(Covariance, lag.max = dims[3], plot = FALSE)$acf,
         by = .(Row, Column)]

ggplot(plotData, aes(x = Sample)) +
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
