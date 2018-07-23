#' GetMissingnessPlots
#'
#' Create plot with overview of missing data.
#'
#' @param inputData Pre-processed input data. Required.
#' @param columnNames Names of columns in \code{inputData} selected for missingness plot. Optional.
#'   Default = \code{c("Transmission", "Migr", "Gender", "Age", "FirstCD4Count")}
#' @param labels Labels to be displayed instead of strings in \code{columnNames}. Optional.
#'   Default = \code{c("Transm.", "Migrant", "Gender", "Age", "CD4")}
#'
#' @return plot object
#'
#' @examples
#' \dontrun{
#' PlotMissingness(inputData)
#' }
#'
#' @export
GetMissingnessPlots <- function(
  inputData,
  columnNames = c("Transmission", "Migr", "Age", "FirstCD4Count"),
  labels = c("Transm.", "Migrant", "Age", "CD4")
)
{
  stopifnot(!missing(inputData))
  stopifnot(length(columnNames) == length(labels))
  stopifnot(length(columnNames) == length(unique(columnNames)))
  stopifnot(length(labels) == length(unique(labels)))

  colors <- c("#69b023", "#d9d9d9")

  getRelFreq <- function(x) sum(x) / length(x)
  isMissing <- function(x) as.integer(is.na(x))
  percent_format <- function(x, decim = 0L) sprintf(paste0("%.", decim, "f%%"), x * 100)

  missData <- inputData[, ..columnNames]
  missData[, c(columnNames) := lapply(.SD, isMissing), .SDcols = columnNames]

  if (nrow(missData) == 0) {
    return(NULL)
  }

  relFreqData <- missData[, lapply(.SD, getRelFreq), .SDcols = columnNames]
  relFreqData <- melt(relFreqData,
                      measure.vars = columnNames,
                      variable.name = "Attribute",
                      variable.factor = FALSE,
                      value.name = "Percentage")
  relFreqData[, Label := labels]
  setorder(relFreqData, -Percentage)

  relFreqPlot <- ggplot(data = relFreqData,
                        aes(x = Attribute, y = Percentage)) +
    geom_col(fill = colors[2]) +
    scale_x_discrete(limits = relFreqData$Attribute, labels = relFreqData$Label) +
    scale_y_continuous(expand = c(0, 0), labels = percent_format) +
    theme_classic() +
    theme(text = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.line = element_line(colour = "#888888"),
          axis.ticks = element_line(colour = "#888888"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) +
    ylab("Relative frequency of missing data")

  patternData <- missData[, .(Percentage = .N / nrow(missData)), by = columnNames]
  setorder(patternData, Percentage)
  patternData[, Combination := do.call(paste, c(.SD, sep = ":")),
              .SDcols = columnNames]

  missPatternData <- melt(patternData,
                          id.vars = c("Percentage", "Combination"),
                          variable.name = "Attribute",
                          variable.factor = FALSE,
                          value.name = "Value")

  missPatternData[, ":="(
    Attribute = factor(Attribute, levels = relFreqData$Attribute),
    Combination = factor(Combination, levels = rev(patternData$Combination))
  )]

  missPatternPlot <- ggplot(data = missPatternData,
                            aes(x = Attribute,
                                y = Combination,
                                fill = factor(Value))) +
    geom_tile(color = "white",
              size = 0.1) +
    theme_minimal() +
    scale_fill_manual(name = "",
                      values = colors) +
    scale_x_discrete(expand = c(0, 0),
                     limits = relFreqData$Attribute,
                     labels = relFreqData$Label) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(text = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text.y = element_blank(),
          legend.position = "none") +
    ylab("Missing data patterns")

  missHistData <- patternData[, .(Combination, Percentage, CombinationId = .I)]
  missHistData[, MissingCategory := factor(ifelse(grepl("1", Combination), "Missing", "Present"),
                                           levels = c("Missing", "Present"))]
  missHistPlot <- ggplot(data = missHistData,
                         aes(x = CombinationId, y = Percentage,
                             fill = MissingCategory)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(name = "", values = setNames(colors, c("Present", "Missing")), drop = FALSE) +
    theme_minimal() +
    scale_x_reverse(expand = c(0, 0),
                    position = "top",
                    breaks = missHistData$CombinationId,
                    labels = percent_format(missHistData$Percentage, 2)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = rep(0, length(labels)),
                       labels = relFreqData$Label) +
    theme(text = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, colour = "white"),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  invisible(list(relFreqPlot, missPatternPlot, missHistPlot))
}
