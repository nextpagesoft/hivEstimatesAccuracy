# 1. INPUT DATA ----------------------------------------------------------------
# Set path to the input data file
# inputDataFilePath <- "g:/My Drive/Projects/19. PZH/Scripts/Received/csv_pilot/PL.csv"
# inputDataFilePath <- "~/share/baza30czer2018_mod_QRT.xlsx"
# inputDataFilePath <- "/media/sf_VirtualBox_Shared/Nikos_test/AT.csv"
inputDataFilePath <- "~/share/dummy_miss1.zip"
# inputDataFilePath <- "~/share/Women_HIV.csv"
# inputDataFilePath <- "~/share/StateTestings/StateData_20181014212132.rds"
# inputDataFilePath <- "/media/sf_VirtualBox_Share/hiv_2017.zip"

diagYearRange <- c(1000, 3000)
notifQuarterRange <- c(1000, 3000)

# 2. ADJUSTMENTS SELECTION -----------------------------------------------------
# Select adjustments to perform. Order is important. Available adjustment names:
# a) "Joint Modelling Multiple Imputation"
# b) " Multiple Imputation using Chained Equations - MICE"
# c) "Reporting Delays"
adjustmentNames <- c(
  # "Joint Modelling Multiple Imputation"
  "Multiple Imputation using Chained Equations - MICE"
  # "Reporting Delays"
)
adjustmentFilePaths <- GetAdjustmentSpecFileNames()
adjustmentSpecs <-
  setNames(lapply(adjustmentNames,
                  function(adjName) GetListObject(adjustmentFilePaths[adjName])),
           adjustmentNames)

# Optionally adjust parameters of the adjustments
# For instance stratify "Reporting Delays" adjustment by Migration category:
adjustmentSpecs[["Multiple Imputation using Chained Equations - MICE"]]$Parameters$nimp$value <- 2
adjustmentSpecs[["Multiple Imputation using Chained Equations - MICE"]]$Parameters$nit$value <- 2
adjustmentSpecs[["Multiple Imputation using Chained Equations - MICE"]]$Parameters$nsdf$value <- 4
adjustmentSpecs[["Multiple Imputation using Chained Equations - MICE"]]$Parameters$imputeRD$value <- FALSE
# adjustmentSpecs[["Reporting Delays"]]$Parameters$startYear$value <- 2000
# adjustmentSpecs[["Reporting Delays"]]$Parameters$endYear$value <- 2017
# adjustmentSpecs[["Reporting Delays"]]$Parameters$endQrt$value <- 1
# adjustmentSpecs[["Reporting Delays"]]$Parameters$stratGender$value <- TRUE
# adjustmentSpecs[["Reporting Delays"]]$Parameters$stratTrans$value <- TRUE
# adjustmentSpecs[["Reporting Delays"]]$Parameters$stratMigr$value <- TRUE

# 3. FULLMIGR MAPPING ----------------------------------------------------------
migrMappingType <- "REPCOUNTRY + UNK + OTHER"

# 4. FINAL REPORT NAME ---------------------------------------------------------
reportName <- "Main Report"
