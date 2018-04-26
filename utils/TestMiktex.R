testDir <- "D:/PdfTest"
inputDataFilePath <- "G:/My Drive/Projects/19. PZH/Scripts/Received/csv_pilot/dummy_miss1.zip"

dir.create(testDir, showWarnings = FALSE, recursive = TRUE)
file.copy(system.file("reports/1.MainReport.Rmd", package = "hivEstimatesAccuracy"),
          file.path(testDir))

# Load main library
library(hivEstimatesAccuracy)
library(shiny)
library(ggplot2)

# Read input data
originalData <- ReadDataFile(inputDataFilePath)

# Pre-process data
defaultValues <- GetPreliminaryDefaultValues()

attrMapping <- GetPreliminaryAttributesMapping(originalData)
if (is.null(attrMapping[["FirstCD4Count"]])) {
  attrMapping[["FirstCD4Count"]] <- "cd4_num"
}

attrMappingStatus <- GetAttrMappingStatus(attrMapping)

if (attrMappingStatus$Valid) {
  inputDataTest <- ApplyAttributesMapping(originalData, attrMapping, defaultValues)
  inputDataTest <- PreProcessInputData(inputDataTest)
  inputDataTestStatus <- GetInputDataValidityStatus(inputDataTest$Table)
  if (inputDataTestStatus$Valid) {
    inputData <- inputDataTest
  } else {
    inputData <- NULL
  }
} else {
  inputData <- NULL
}

# Set the list of adjustments.
adjustmentFilePaths <- GetAdjustmentSpecFileNames()
adjustmentSpecs <- list(
  GetListObject(adjustmentFilePaths["Reporting Delays"])
)

# Run adjustments
adjustedData <- RunAdjustments(data = inputData$Table,
                               adjustmentSpecs = adjustmentSpecs)

# Create report
reportFilePaths <- GetReportFileNames()
params <- list(AdjustedData = adjustedData,
               ReportingDelay = 0,
               Smoothing = 0)

reportFileName <- rmarkdown::render(input = file.path(testDir, "1.MainReport.Rmd"),
                                    output_format = "pdf_document",
                                    runtime = "static",
                                    run_pandoc = TRUE,
                                    clean = FALSE,
                                    quiet = FALSE,
                                    envir = new.env(parent = globalenv()),
                                    params = params)
