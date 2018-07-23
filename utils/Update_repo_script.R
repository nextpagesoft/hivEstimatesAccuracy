# Documentation, testing, check
devtools::document()
devtools::check()

# Build source and binary versions
repoPath <- "d:/_REPOSITORIES/hivEstimatesAccuracy_deploy"
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)
devtools::build(path = repoPath, binary = FALSE)
devtools::build(path = repoPath, binary = TRUE, args = c('--preclean'))

# Read new version string
descr <- as.data.frame(read.dcf(file = "DESCRIPTION"))
version <- as.character(descr$Version)

# Copy package files to appropriate subfolders
file.copy(file.path(repoPath, paste0("hivEstimatesAccuracy_", version, ".tar.gz")),
          file.path(repoPath, "repo/src/contrib", paste0("hivEstimatesAccuracy_", version, ".tar.gz")),
          overwrite = TRUE)
file.copy(file.path(repoPath, paste0("hivEstimatesAccuracy_", version, ".zip")),
          file.path(repoPath, "repo/bin/windows/contrib/3.5", paste0("hivEstimatesAccuracy_", version, ".zip")),
          overwrite = TRUE)
unlink(file.path(repoPath, paste0("hivEstimatesAccuracy_", version, ".zip")))

# Update repo metafiles
tools::write_PACKAGES(dir = file.path(repoPath, "repo/src/contrib"), type = "source")
tools::write_PACKAGES(dir = file.path(repoPath, "repo/bin/windows/contrib/3.5"), type = "win.binary")

# Update currect version string
writeLines(version, file.path(repoPath, "version.txt"))
