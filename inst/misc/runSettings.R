# 1. INPUT DATA ------------------------------------------------------------------------------------
# Set path to the input data file
inputDataFilePath <- "/media/sf_VirtualBox_SharedDrive/dummy_miss1.zip"

# 2. ADJUSTMENTS SELECTION -------------------------------------------------------------------------
# Select adjustments to perform. Order is important. Available adjustment names:
# a) "Multiple Imputations (jomo)"
# b) "Multiple Imputations (mice)"
# c) "Reporting Delays"
adjustmentNames <- c(
  "Multiple Imputations (mice)",
  "Reporting Delays"
)
adjustmentFilePaths <- GetAdjustmentSpecFileNames()
adjustmentSpecs <- setNames(lapply(adjustmentNames,
                                   function(adjName) GetListObject(adjustmentFilePaths[adjName])),
                            adjustmentNames)

# Optionally adjust parameters of the adjustments:
# For instance stratify "Reporting Delays" adjustment by Transmission category
adjustmentSpecs[["Reporting Delays"]]$Parameters$stratTrans$value <- TRUE
