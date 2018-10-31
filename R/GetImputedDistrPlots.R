#' GetImputedDistPlots
#'
#' Get density/proportion plot showing observed and imputed data
#'
#' @param colName Name of column to show density/proportion. Required.
#' @param dt Data table object. Required.
#' @param missingOnly Logical indicating to show only imputed values in
#'   the imputed data sets. Optional. Default = \code{TRUE}.
#' @param colorPalette Character vector of colors for plotted
#'  series. Optional. Default = \code{c("#69b023", "#7bbcc0")}.
#'
#' @return ggplot2 object
#'
#' @examples
#' \dontrun{
#' GetImputedDistPlots(dt, colName, missingOnly, colorPalette)
#' }
#'
#' @export
GetImputedDistPlots <- function(
  colName,
  dt,
  missingOnly = TRUE,
  colorPalette = c("#69b023", "#7bbcc0")
) {

  imps <- dt[, unique(Imputation)]
  numImps <- length(imps) - 1L
  numUsedImps <- min(numImps, 5)
  isFactor <- dt[, is.factor(get(colName))]

  selMiss <- dt[Imputation == 0][, is.na(get(colName))]
  selObserved <- rep(TRUE, length(selMiss))
  if (missingOnly) {
    selImputed <- c(rep(selMiss, numUsedImps),
                    rep(FALSE, length(selMiss) * (numImps - numUsedImps)))
    dataDesc <- " on missing data only"
  } else {
    selImputed <- c(rep(selObserved, numUsedImps),
                    rep(FALSE, length(selMiss) * (numImps - numUsedImps)))
    dataDesc <- " on all data"
  }
  miMiss <- dt[c(selObserved, selImputed)]
  miMiss[, Imputation := factor(Imputation)]

  labels    <- ifelse(imps == 0, "Observed", sprintf("Chain %s", imps))
  linetypes <- ifelse(imps == 0, "solid", "dotted")
  sizes     <- ifelse(imps == 0, 1, 0.5)
  colors    <- ifelse(imps == 0, colorPalette[1], colorPalette[2])

  if (isFactor) {
    miMiss <- miMiss[, .(Count = .N), by = c("Imputation", colName)]
    miMiss[, ImputationTotal := sum(Count), by = .(Imputation)]
    miMiss[, Dist := Count / ImputationTotal]

    p <- ggplot(data = miMiss) +
      geom_point(
        mapping = aes(
          x = get(colName),
          y = Dist,
          color = Imputation,
          group = Imputation
        ),
        position = 'identity',
        size = 2) +
      geom_line(
        mapping = aes(
          x = get(colName),
          y = Dist,
          color = Imputation,
          group = Imputation
        ),
        linetype = 'dotted',
        position = 'identity',
        size = 0.5) +
      scale_color_manual(name = "Data",
                         values = colors,
                         breaks = imps,
                         labels = labels) +
      ggtitle(sprintf("Proportion of observed and imputed values%s\n", dataDesc)) +
      ylab("Proportion")
  } else {
    p <- ggplot(data = miMiss) +
      stat_density(
        mapping = aes(
          x = get(colName),
          color = Imputation,
          linetype = Imputation,
          size = Imputation
        ),
        geom = 'line',
        position = 'identity',
        adjust = 1) +
      scale_color_manual(name = "Data", values = colors, breaks = imps, labels = labels) +
      scale_linetype_manual(name = "Data", values = linetypes, breaks = imps, labels = labels) +
      scale_size_manual(name = "Data", values = sizes, breaks = imps, labels = labels) +
      scale_x_continuous(expand = c(0, 0)) +
      ggtitle(sprintf("Density of observed and imputed values%s\n", dataDesc)) +
      xlab(colName) +
      ylab("Density")
  }

  p <- p +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic() +
    theme(plot.title = element_text(size = 12, face = "plain"),
          text = element_text(size = 12, face = "plain"),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "#888888"),
          axis.ticks = element_line(colour = "#888888")) +
    xlab(colName)

  return(p)
}
