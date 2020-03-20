library(miniCRAN)

repoCRAN <- "https://cran.r-project.org/"

pkgs <- c(
  "callr", "DT", "data.table", "foreign", "future", "grid", "ggplot2", "graphics", "jomo", "knitr",
  "mice", "mitools", "quantreg", "parallel", "readxl", "rmarkdown", "scales", "shiny",
  "shinycssloaders", "shinydashboard", "shinyjs", "SparseM", "stats", "survival", "tools", "utils",
  "yaml", "xml2"
)
pkgs <- setdiff(pkgs, c("grid", "graphics", "parallel", "stats", "tools", "utils"))

pkgList <- pkgDep(pkgs, repos = repoCRAN, type = "source", suggests = FALSE)

repoPath <- "d:/_DEPLOYMENT/hivEstimatesAccuracy/repo"
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)

makeRepo(pkgList, path = repoPath, repos = repoCRAN, type = c("source", "win.binary"))
