# 1. INPUT DATA ----------------------------------------------------------------
# Set path to the input data file
# inputDataFilePath <- "g:/My Drive/Projects/19. PZH/Scripts/Received/csv_pilot/PL.csv"
# inputDataFilePath <- "/media/sf_VirtualBox_SharedDrive/dummy_miss1.zip"
inputDataFilePath <- "/media/sf_VirtualBox_Share/hiv_2017.zip"

# 2. ADJUSTMENTS SELECTION -----------------------------------------------------
# Select adjustments to perform. Order is important. Available adjustment names:
# a) "Joint Modelling Multiple Imputation"
# b) " Multiple Imputation using Chained Equations - MICE"
# c) "Reporting Delays"
adjustmentNames <- c(
  "Multiple Imputation using Chained Equations - MICE",
  "Reporting Delays"
)
adjustmentFilePaths <- GetAdjustmentSpecFileNames()
adjustmentSpecs <-
  setNames(lapply(adjustmentNames,
                  function(adjName) GetListObject(adjustmentFilePaths[adjName])),
           adjustmentNames)

# Optionally adjust parameters of the adjustments
# For instance stratify "Reporting Delays" adjustment by Transmission category:
# adjustmentSpecs[["Reporting Delays"]]$Parameters$stratTrans$value <- TRUE

# 3. FULLMIGR MAPPING ----------------------------------------------------------
migrMappingType <- "REPCOUNTRY + UNK + 4 most prevalent other regions"

# 4. FINAL REPORT NAME ---------------------------------------------------------
reportName <- "Main Report-new"
