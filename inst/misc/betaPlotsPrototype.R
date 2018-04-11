i <- 1
PlotMCMCDiagnostic(plotData = adjustedData[[i]]$Artifacts$F$Beta)

plot(adjustedData[[i]]$Artifacts$F$Beta[1, 1, ])
plot(plotData[Row == "AIDS" & Column == "Age", Beta])

plotData <- data.table::setDT(as.data.frame.table(adjustedData[[i]]$Artifacts$F$Beta,
                                                  responseName = "Value"))
plotData[, ":="(
  Var3 = as.integer(Var3)
)]

setnames(plotData, c("Row", "Column", "Sample", "Beta"))

plotData[, Element := paste(Row, Column, sep = " / ")]

ggplot(plotData, aes(x = Sample)) +
  geom_line(aes(y = Beta), colour = "#69b023") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Betas") +
  facet_wrap(~Element,
             scales = "free_y",
             strip.position = "top",
             nrow = 7,
             ncol = 5) +
  theme(strip.background = element_rect(fill = "#e9e9e9",
                                        linetype = "blank"),
        strip.placement = "outside",
        strip.text = element_text(size = 9),
        plot.title = element_text(size = 13),
        axis.title.x =  element_text(size = 10),
        axis.title.y =  element_text(size = 10),
        text = element_text(size = 11),
        panel.border = element_rect(colour = "#888888"),
        axis.line = element_line(colour = "#888888"),
        axis.ticks = element_line(colour = "#888888"))
