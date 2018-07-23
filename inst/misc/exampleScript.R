# 1. LOAD LIBRARY ----------------------------------------------------------------------------------
library(hivEstimatesAccuracy)

# 2. LOAD SETTINGS ---------------------------------------------------------------------------------
source("./inst/misc/runSettings.R")

# 3. READ INPUT DATA -------------------------------------------------------------------------------
originalData <- ReadDataFile(inputDataFilePath)

# 4. PRE-PROCESS DATA ------------------------------------------------------------------------------
attrMapping <- GetPreliminaryAttributesMapping(originalData)
# Map "FirstCD4Count" to "cd4_num" if not mapped yet
if (is.null(attrMapping[["FirstCD4Count"]])) {
  attrMapping[["FirstCD4Count"]] <- "cd4_num"
}
attrMappingStatus <- GetAttrMappingStatus(attrMapping)
if (attrMappingStatus$Valid) {
  inputDataTest <- ApplyAttributesMapping(originalData,
                                          attrMapping,
                                          GetPreliminaryDefaultValues())
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

if (!is.null(inputData)) {
  # Get "Summary" page plots (optionally)
  summaryArtifacts <- GetDataSummaryArtifacts(inputData = inputData$Table)

  # 5. RUN ADJUSTMENTS -------------------------------------------------------------------------------
  adjustedData <- RunAdjustments(data = inputData$Table,
                                 adjustmentSpecs = adjustmentSpecs)

  # 6. SAVE ADJUSTED DATA ----------------------------------------------------------------------------
  # Take the last adjustment output as final data
  finalData <- adjustedData[[length(adjustedData)]][["Table"]]

  # Write output
  outputDataFilePath <- CreateOutputFileName(inputDataFilePath,
                                             suffix = paste0("_", GetTimeStamp()))
  WriteDataFile(finalData, outputDataFilePath)

  # 7. CREATE FINAL REPORT (OPTIONALLY) --------------------------------------------------------------
  reportFilePath <- GetReportFileNames()["Main Report"]
  params <- list(AdjustedData = adjustedData,
                 ReportingDelay = TRUE,
                 Smoothing = FALSE)

  htmlReportFileName <- RenderReportToFile(
    filePath = reportFilePath,
    format = "html_document",
    params = params,
    outDir = dirname(inputDataFilePath))
  browseURL(htmlReportFileName)

  pdfReportFileName <- RenderReportToFile(
    filePath = reportFilePath,
    format = "pdf_document",
    params = params,
    outDir = dirname(inputDataFilePath))
  browseURL(pdfReportFileName)

  wordReportFileName <- RenderReportToFile(
    filePath = reportFilePath,
    format = "word_document",
    params = params,
    outDir = dirname(inputDataFilePath))
  browseURL(wordReportFileName)
}
