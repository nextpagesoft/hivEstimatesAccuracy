rVersion <- "3.6"

setwd('D:/_REPOSITORIES/hivModelling/')
renv::activate()
renv::restore()

# Documentation, testing, check
devtools::document()

# Build source and binary versions
repoPath <- "d:/_DEPLOYMENT/hivEstimatesAccuracy/pkgBuilds"
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)
devtools::build(path = repoPath,
                binary = FALSE)
devtools::build(path = repoPath,
                binary = TRUE,
                args = c('--preclean'))

# Read new version string
descr <- as.data.frame(read.dcf(file = "DESCRIPTION"))
version <- as.character(descr$Version)

tarFileName <- paste0("hivModelling_", version, ".tar.gz")
zipFileName <- paste0("hivModelling_", version, ".zip")

# Copy package files to appropriate subfolders
file.copy(file.path(repoPath, tarFileName),
          file.path(repoPath, "..", "repo", "src", "contrib", tarFileName),
          overwrite = TRUE)
file.copy(file.path(repoPath, zipFileName),
          file.path(repoPath, "..", "repo", "bin", "windows", "contrib", rVersion, zipFileName),
          overwrite = TRUE)



setwd('D:/_REPOSITORIES/hivEstimatesAccuracy/')
renv::activate()
renv::restore()

# Documentation, testing, check
devtools::document()

# Build source and binary versions
repoPath <- "d:/_DEPLOYMENT/hivEstimatesAccuracy/pkgBuilds"
dir.create(repoPath, showWarnings = FALSE, recursive = TRUE)
devtools::build(path = repoPath,
                binary = FALSE)
devtools::build(path = repoPath,
                binary = TRUE,
                args = c('--preclean'))

# Read new version string
descr <- as.data.frame(read.dcf(file = "DESCRIPTION"))
version <- as.character(descr$Version)

tarFileName <- paste0("hivEstimatesAccuracy_", version, ".tar.gz")
zipFileName <- paste0("hivEstimatesAccuracy_", version, ".zip")

# Copy package files to appropriate subfolders
file.copy(file.path(repoPath, tarFileName),
          file.path(repoPath, "..", "repo", "src", "contrib", tarFileName),
          overwrite = TRUE)
file.copy(file.path(repoPath, zipFileName),
          file.path(repoPath, "..", "repo", "bin", "windows", "contrib", rVersion, zipFileName),
          overwrite = TRUE)

# Update repo metafiles
tools::write_PACKAGES(dir = file.path(repoPath, "..", "repo", "src", "contrib"), type = "source")
tools::write_PACKAGES(dir = file.path(repoPath, "..", "repo", "bin", "windows", "contrib", rVersion), type = "win.binary")

# Update currect version string
writeLines(version, file.path(repoPath, "version.txt"))

# # Upload to server
# UploadFiles <- function(fileNames, sourcePath, destPath) {
#   sapply(fileNames, function(fileName) {
#     RCurl::ftpUpload(what = file.path(sourcePath, fileName),
#                      to = file.path(destPath, fileName),
#                      userpwd = userpwd)
#   })
# }

# ftpPath <- keyring::key_get("HIV-server", "ftpPath")
# userpwd <- keyring::key_get("HIV-server", "userpwd")
#
# UploadFiles(fileNames = c("version.txt", tarFileName),
#             sourcePath = repoPath,
#             destPath = ftpPath)
#
# UploadFiles(fileNames = c("PACKAGES.rds", "PACKAGES.gz", "PACKAGES", tarFileName),
#             sourcePath = file.path(repoPath, "CRAN", "src", "contrib"),
#             destPath = file.path(ftpPath, "repo", "src", "contrib"))
#
# UploadFiles(fileNames = c("PACKAGES.rds", "PACKAGES.gz", "PACKAGES", zipFileName),
#             sourcePath = file.path(repoPath, "CRAN", "bin", "windows", "contrib", "3.5"),
#             destPath = file.path(ftpPath, "repo", "bin", "windows", "contrib", "3.5"))
