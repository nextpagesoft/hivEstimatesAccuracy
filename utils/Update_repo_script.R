# Documentation, testing, check
devtools::document()

# Build source and binary versions
repoPath <- "d:/_DEPLOYMENT/hivEstimatesAccuracy/miniCRAN"
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)
devtools::build(path = file.path(repoPath, "pkgBuilds"), binary = FALSE)
devtools::build(path = file.path(repoPath, "pkgBuilds"), binary = TRUE, args = c('--preclean'))

# Read new version string
descr <- as.data.frame(read.dcf(file = "DESCRIPTION"))
version <- as.character(descr$Version)

# Copy package files to appropriate subfolders
file.copy(file.path(repoPath, "pkgBuilds", paste0("hivEstimatesAccuracy_", version, ".tar.gz")),
          file.path(repoPath, "CRAN/src/contrib", paste0("hivEstimatesAccuracy_", version, ".tar.gz")),
          overwrite = TRUE)
file.copy(file.path(repoPath, "pkgBuilds", paste0("hivEstimatesAccuracy_", version, ".zip")),
          file.path(repoPath, "CRAN/bin/windows/contrib/3.5", paste0("hivEstimatesAccuracy_", version, ".zip")),
          overwrite = TRUE)

# Update repo metafiles
tools::write_PACKAGES(dir = file.path(repoPath, "CRAN/src/contrib"), type = "source")
tools::write_PACKAGES(dir = file.path(repoPath, "CRAN/bin/windows/contrib/3.5"), type = "win.binary")

# Update currect version string
writeLines(version, file.path(repoPath, "pkgBuilds", "version.txt"))
