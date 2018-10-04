# A: Set some general settings ---------------------------------------------------------------------
baseUrl <- "http://nextpagesoft.net/hiv-estimates-accuracy/"
baseFileName <- "hivEstimatesAccuracy_%s.tar.gz"

# B: Get latest version number ---------------------------------------------------------------------
conn <- base::url(file.path(baseUrl, "version.txt"))
latestPkgVersion <- readLines(conn)
close(conn)

# C: Determine if installation of new version is required ------------------------------------------
installPkg <- TRUE
if (require("hivEstimatesAccuracy", quietly = TRUE)) {
  installedPkgVersion <- utils::packageVersion("hivEstimatesAccuracy")

  # If installed version is different than the latest version then remove it
  if (latestPkgVersion != installedPkgVersion) {
    # Remove old version
    remove.packages("hivEstimatesAccuracy")
  } else {
    installPkg <- FALSE
  }
}

# D: Install "hivEstimatesAccuracy" package if needed ----------------------------------------------
if (installPkg) {
  # Install package "devtools" if not installed already
  if (!require("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }

  # Download "hivEstimatesAccuracy" package
  devtools::install_url(file.path(baseUrl, sprintf(baseFileName, latestPkgVersion)))
}

# E: Run application -------------------------------------------------------------------------------
hivEstimatesAccuracy::RunApp()
