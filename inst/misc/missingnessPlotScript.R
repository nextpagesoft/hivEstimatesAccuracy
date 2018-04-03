library(data.table)
colNames <- c("FirstCD4Count", "Transmission", "Migr", "Age", "Gender")

getRelFreq <- function(x) sum(x) / length(x)
isMissing <- function(x) as.integer(is.na(x))

missData <- inputData[, ..colNames]
missData[, c(colNames) := lapply(.SD, isMissing), .SDcols = colNames]

relFreqData <- missData[, lapply(.SD, getRelFreq), .SDcols = colNames]
relFreqData <- melt(relFreqData,
                    measure.vars = colNames,
                    variable.name = "Attribute",
                    value.name = "Percentage")

relFreqPlot <- ggplot(data = relFreqData,
                      aes(x = Attribute, y = Percentage)) +
  geom_col(fill = "red") +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  ylab("Proportion of missings")
relFreqPlot

patternData <- missData[, .(Percentage = .N / nrow(missData)), by = colNames]
setorder(patternData, Percentage)
patternData[, Combination := do.call(paste, c(.SD, sep = ":")), .SDcols = colNames]

missPatternData <- melt(patternData,
                        id.vars = c("Percentage", "Combination"),
                        variable.name = "Attribute",
                        value.name = "Value")

missPatternPlot <- ggplot(data = missPatternData,
                          aes(x = Attribute,
                              y = Combination,
                              fill = factor(Value))) +
  geom_tile(color = "white",
            size = 0.1) +
  theme_minimal() +
  scale_fill_manual(name = "",
                    values = c("blue", "red"),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),
        legend.position = "none") +
  ylab("Combinations")
missPatternPlot

missHistData <- patternData[, .(Combination, Percentage, CombinationId = .I)]
missHistData[, MissingCategory := grepl("1", Combination)]

missHistPlot <- ggplot(data = missHistData,
                       aes(x = CombinationId, y = Percentage, fill = MissingCategory)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual("Counts", values = c("blue", "red")) +
  theme_minimal() +
  scale_x_reverse(expand = c(0, 0),
                  position = "top",
                  breaks = missHistData$CombinationId,
                  labels = scales::percent(missHistData$Percentage)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = rep(0, length(colNames)),
                     labels = colNames) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, colour = "white"),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

PlotMultipleCharts(list(relFreqPlot, missPatternPlot, missHistPlot), cols = 3)
