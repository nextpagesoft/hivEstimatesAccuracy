#' PlotMultipleCharts
#'
#' Plot multiple ggplot objects in a grid
#'
#' @param plots List of ggplot objects. Required.
#' @param cols Number of columns. Optional. Default = 1.
#' @param layout A matrix specifying layout. If present, \code{cols} is ignored. Optional. Default = NULL.
#' @param widths Vector of column widths. Default = NULL
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' PlotMultipleCharts(plots)
#' }
#'
#' @export
PlotMultipleCharts <- function(plots, cols = 1, layout = NULL, widths = NULL)
{
  if (length(plots) == 0) {
    return(invisible(NULL))
  }

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (is.null(widths)) {
    widths <- rep(1, ncol(layout))
  }

  if (numPlots == 1) {
    print(plots[[1]])
  } else if (numPlots > 1) {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout),
                                               ncol(layout),
                                               widths = widths)))

    # Make each plot, in the correct location
    for (i in seq_len(numPlots)) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchIdx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchIdx$row,
                                      layout.pos.col = matchIdx$col))
    }
  }
}
