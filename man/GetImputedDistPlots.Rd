% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetImputedDistrPlots.R
\name{GetImputedDistPlots}
\alias{GetImputedDistPlots}
\title{GetImputedDistPlots}
\usage{
GetImputedDistPlots(
  colName,
  dt,
  missingOnly = TRUE,
  colorPalette = c("#69b023", "#7bbcc0")
)
}
\arguments{
\item{colName}{Name of column to show density/proportion. Required.}

\item{dt}{Data table object. Required.}

\item{missingOnly}{Logical indicating to show only imputed values in
the imputed data sets. Optional. Default = \code{TRUE}.}

\item{colorPalette}{Character vector of colors for plotted
series. Optional. Default = \code{c("#69b023", "#7bbcc0")}.}
}
\value{
ggplot2 object
}
\description{
Get density/proportion plot showing observed and imputed data
}
\examples{
\dontrun{
GetImputedDistPlots(dt, colName, missingOnly, colorPalette)
}

}
