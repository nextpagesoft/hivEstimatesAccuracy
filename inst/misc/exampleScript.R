# 1. LOAD LIBRARY ----------------------------------------------------------------------------------
library(hivEstimatesAccuracy)

# 2. LOAD SETTINGS ---------------------------------------------------------------------------------
# source("D:/_REPOSITORIES/Github/hivEstimatesAccuracy/inst/misc/runSettings.R")
source("/home/daniel/_REPOSITORIES/hivEstimatesAccuracy/inst/misc/runSettings.R")

# 3. READ INPUT DATA -------------------------------------------------------------------------------
uploadedData <- ReadDataFile(inputDataFilePath)
if (GetIsState(uploadedData)) {
  originalData <- uploadedData$OriginalData
} else {
  originalData <- uploadedData
}

# 4. PRE-PROCESS DATA ------------------------------------------------------------------------------
attrMapping <- GetPreliminaryAttributesMapping(originalData)

if (is.null(attrMapping[["RecordId"]])) {
  attrMapping[["RecordId"]] <- "Identyfikator"
}
if (is.null(attrMapping[["Age"]])) {
  attrMapping[["Age"]] <- "WiekHIVdor"
}

# Map "FirstCD4Count" to "cd4_num" if not mapped yet
if (is.null(attrMapping[["FirstCD4Count"]])) {
  attrMapping[["FirstCD4Count"]] <- "cd4_num"
}
attrMappingStatus <- GetAttrMappingStatus(attrMapping)
if (attrMappingStatus[["Valid"]]) {
  inputDataTest <- ApplyAttributesMapping(originalData,
                                          attrMapping,
                                          GetPreliminaryDefaultValues())
  inputDataTest <- PreProcessInputDataBeforeSummary(inputData = inputDataTest)
  inputDataTestStatus <- GetInputDataValidityStatus(inputDataTest$Table)
  if (inputDataTestStatus[["Valid"]]) {
    inputData <- inputDataTest
  } else {
    inputData <- NULL
  }
} else {
  inputData <- NULL
}

if (!is.null(inputData)) {
  # Apply GroupedRegionOfOrigin mapping
  distr <- GetOriginDistribution(inputData$Table)
  map <- GetOriginGroupingMap(migrMappingType, distr)
  inputData <- ApplyOriginGroupingMap(inputData, map)

  # Get "Summary" page plots (optionally)
  GetDiagnosisYearDensityPlot(plotData = inputData$Table)
  GetNotificationQuarterDensityPlot(plotData = inputData$Table)
  summaryInputData <- inputData$Table
  PreProcessInputDataBeforeAdjustments(summaryInputData)
  summaryArtifacts <- GetDataSummaryArtifacts(inputData = inputData$Table)

  # 5. RUN ADJUSTMENTS -----------------------------------------------------------------------------
  adjustedData <- RunAdjustments(data = inputData$Table,
                                 adjustmentSpecs = adjustmentSpecs,
                                 diagYearRange = NULL,
                                 notifQuarterRange = NULL,
                                 seed = NULL)

  # 6. SAVE ADJUSTED DATA --------------------------------------------------------------------------
  # Take the last adjustment output as final data
  finalData <- adjustedData[[length(adjustedData)]][["Table"]]

  # Write output
  outputDataFilePath <- CreateOutputFileName(inputDataFilePath,
                                             suffix = paste0("_", GetTimeStamp()))
  WriteDataFile(finalData, outputDataFilePath)

  # 7. CREATE FINAL REPORT (OPTIONALLY) ------------------------------------------------------------
  reportFilePath <- GetReportFileNames()[reportName]
  params <- list(AdjustedData = adjustedData,
                 ReportingDelay = TRUE,
                 Smoothing = FALSE,
                 CD4ConfInt = TRUE)

  if (is.element(reportName, c("Main Report"))) {
    params <- GetMainReportArtifacts(params)
  }

  params <- modifyList(params,
                       list(Artifacts =
                              list(FileName = inputDataFilePath,
                                   DiagYearRange = c(2000, 2010),
                                   DiagYearRangeApply = TRUE)))

  htmlReportFileName <- RenderReportToFile(
    reportFilePath = reportFilePath,
    format = "html_document",
    params = params,
    outDir = dirname(inputDataFilePath))
  browseURL(htmlReportFileName)

  latexReportFileName <- RenderReportToFile(
    reportFilePath = reportFilePath,
    format = "latex_document",
    params = params,
    outDir = dirname(inputDataFilePath))

  pdfReportFileName <- RenderReportToFile(
    reportFilePath = reportFilePath,
    format = "pdf_document",
    params = params,
    outDir = dirname(inputDataFilePath))
  browseURL(pdfReportFileName)

  wordReportFileName <- RenderReportToFile(
    reportFilePath = reportFilePath,
    format = "word_document",
    params = params,
    outDir = dirname(inputDataFilePath))
  browseURL(wordReportFileName)
}
