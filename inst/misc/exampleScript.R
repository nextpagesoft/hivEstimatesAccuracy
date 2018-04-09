# Load main library
library(hivEstimatesAccuracy)
library(shiny)
library(ggplot2)

# Define input data path
inputDataFilePath <- "D:/Drive/Projects/19. PZH/Scripts/Received/20180111/dummy_miss1.csv"

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
  inputDataTestStatus <- GetInputDataValidityStatus(inputDataTest)
  if (inputDataTestStatus$Valid) {
    inputData <- inputDataTest
  } else {
    inputData <- NULL
  }
} else {
  inputData <- NULL
}

inputData <- PreProcessInputData(inputData)

summaryArtifacts <- GetDataSummaryArtifacts(inputData)

# Read adjustment specifications. Take a specific one, "Multiple imputation"
adjustmentFilePaths <- GetAdjustmentSpecFileNames()
adjustmentSpecs <- list(
  GetListObject(adjustmentFilePaths["Multiple Imputations (mice)"]),
  # GetListObject(adjustmentFilePaths["Multiple Imputations (jomo)"])
  GetListObject(adjustmentFilePaths["Reporting Delays"])
)

# Run diagnostic (on the first adjustment only, this is for illustration)
for (adjustmentSpec in adjustmentSpecs) {
  parameters <- GetParamInfoFromAdjustSpec(adjustmentSpec$Parameters,
                                           infoType = "value")

  print(RenderReportForAdjSpec(adjustmentSpec = adjustmentSpec,
                               fileNameSuffix = "diagnostic",
                               params = list(Table = inputData,
                                             Parameters = parameters)))
}

# Run adjustments
adjustedData <- RunAdjustments(data = inputData,
                               adjustmentSpecs = adjustmentSpecs)

intermReport <- ""
for (i in seq_len(length(adjustmentSpecs))) {
  intermReport <- paste(intermReport,
                        RenderReportForAdjSpec(adjustmentSpec = adjustmentSpecs[[i]],
                                               fileNameSuffix = "intermediate",
                                               params = adjustedData[[i]]))
}
print(HTML(intermReport))

# Create report
reportFilePaths <- GetReportFileNames()
params <- list(AdjustedData = adjustedData)

reportFileName <- RenderReportToFile(filePath = reportFilePaths["Multiple Imputations"],
                                     format = "html_fragment",
                                     params = params)

reportFileName <- RenderReportToFile(filePath = reportFilePaths["Multiple Imputations"],
                                     format = "html_document",
                                     params = params)

reportFileName <- RenderReportToFile(filePath = reportFilePaths["Multiple Imputations"],
                                     format = "pdf_document",
                                     params = params)

reportFileName <- RenderReportToFile(filePath = reportFilePaths["Multiple Imputations"],
                                     format = "word_document",
                                     params = params)

# Take the last adjustment output as final data
finalData <- adjustedData[[length(adjustedData)]]

# Write output
outputDataFilePath <- CreateOutputFileName(inputDataFilePath,
                                           suffix = paste0("_", GetTimeStamp()))
WriteDataFile(finalData$Table, outputDataFilePath)
