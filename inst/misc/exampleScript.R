# Load main library
library(hivEstimatesAccuracy)
library(shiny)
library(ggplot2)

# Define input data path
# inputDataFilePath <- "G:/My Drive/Projects/19. PZH/Scripts/Received/PLtest.csv"
inputDataFilePath <- "G:/My Drive/Projects/19. PZH/Scripts/Received/csv_pilot/dummy_miss1.csv"

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

summaryArtifacts <- GetDataSummaryArtifacts(inputData = inputData$Table)

# Read adjustment specifications. Take a specific one, "Multiple imputation"
adjustmentFilePaths <- GetAdjustmentSpecFileNames()
adjustmentSpecs <- list(
  # GetListObject(adjustmentFilePaths["Multiple Imputations (mice)"]),
  # GetListObject(adjustmentFilePaths["Multiple Imputations (jomo)"])
  GetListObject(adjustmentFilePaths["Reporting Delays"])
)

adjustmentSpecs[[1]]$Parameters$stratTrans$value <- TRUE

# Run adjustments
adjustedData <- RunAdjustments(data = inputData$Table,
                               adjustmentSpecs = adjustmentSpecs)

# Render intermediate report to file and open it in external browser
intermReportFilePath <- RenderReportToFile(
  filePath = system.file("reports/intermediate/2.RD_intermediate.Rmd",
                         package = "hivEstimatesAccuracy"),
  params = list(InputData = adjustedData[[2]]))
browseURL(intermReportFilePath)
unlink(intermReportFilePath)

intermReport <- RenderReportToHTML(
  filePath = system.file("reports/intermediate/0.PreProcess.Rmd",
                         package = "hivEstimatesAccuracy"),
  params = list(InputData = inputData))
for (i in seq_along(adjustmentSpecs)) {
  intermReport <- paste(intermReport,
                        RenderReportForAdjSpec(adjustmentSpec = adjustmentSpecs[[i]],
                                               fileNameSuffix = "intermediate",
                                               params = adjustedData[[i]]))
}
intermReport <- HTML(intermReport)

# Create report
reportFilePaths <- GetReportFileNames()
params <- list(AdjustedData = adjustedData,
               ReportingDelay = 0,
               Smoothing = 0)

reportFileName <- RenderReportToFile(filePath = reportFilePaths["Main Report"],
                                     format = "html_fragment",
                                     params = params)
browseURL(reportFileName)

reportFileName <- RenderReportToFile(filePath = reportFilePaths["Main Report"],
                                     format = "html_document",
                                     params = params)

reportFileName <- RenderReportToFile(filePath = reportFilePaths["Main Report"],
                                     format = "pdf_document",
                                     params = params)

reportFileName <- RenderReportToFile(filePath = reportFilePaths["Main Report"],
                                     format = "word_document",
                                     params = params)

# Take the last adjustment output as final data
finalData <- adjustedData[[length(adjustedData)]]

# Write output
outputDataFilePath <- CreateOutputFileName(inputDataFilePath,
                                           suffix = paste0("_", GetTimeStamp()))
WriteDataFile(finalData$Table, outputDataFilePath)
