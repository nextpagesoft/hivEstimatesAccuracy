# 1. LOAD LIBRARY ----------------------------------------------------------------------------------
library(hivModelling)
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

attrMapping[["FirstCD4Count"]] <- "cd4_num"

attrMappingStatus <- GetAttrMappingStatus(attrMapping)
if (attrMappingStatus[["Valid"]]) {
  defaultValues <- GetPreliminaryDefaultValues()
  inputDataTest <- ApplyAttributesMapping(originalData,
                                          attrMapping,
                                          defaultValues)
  inputDataTest <- PreProcessInputDataBeforeSummary(inputData = inputDataTest)
  inputDataTestStatus <- GetInputDataValidityStatus(inputData = inputDataTest$Table)
  if (inputDataTestStatus[["Valid"]]) {
    inputData <- inputDataTest
  } else {
    inputData <- NULL
  }
} else {
  inputData <- NULL
}

if (!is.null(inputData)) {
  hivModelData <- PrepareDataSetsForModel(
    inputData$Table,
    splitBy = 'Imputation',
    strata = c('Gender', 'Transmission'),
    listIndex = 0
  )
  context <- GetRunContext(data = hivModelData[[1]])
  data <- GetPopulationData(context)
  modelResults <- PerformMainFit(context, data)
  modelPlots <- CreateOutputPlots(modelResults)

  # Apply GroupedRegionOfOrigin mapping
  map <- GetOriginGroupingMap(migrMappingType, GetOriginDistribution(inputData$Table))

  map[FullRegionOfOrigin %in% c("CENTEUR", "EASTEUR", "EUROPE", "WESTEUR"),
      GroupedRegionOfOrigin := "EUROPE"]
  map[FullRegionOfOrigin %in% c("SUBAFR"),
      GroupedRegionOfOrigin := "SUBAFR"]
  map[GroupedRegionOfOrigin %in% c("OTHER"),
      GroupedRegionOfOrigin := FullRegionOfOrigin]

  inputData <- ApplyOriginGroupingMap(inputData, map)

  # Get "Summary" page plots (optionally)
  inputData$Table <- inputData$Table[
    is.na(DateOfDiagnosisYear) |
      is.na(NotificationTime) |
      (DateOfDiagnosisYear %between% diagYearRange & NotificationTime %between% notifQuarterRange)
  ]

  GetDiagnosisYearDensityPlot(plotData = inputData$Table)
  GetNotificationQuarterDensityPlot(plotData = inputData$Table)
  summaryInputData <- inputData$Table
  PreProcessInputDataBeforeAdjustments(inputData = summaryInputData)
  summaryArtifacts <- GetDataSummaryArtifacts(inputData = summaryInputData)

  # 5. RUN ADJUSTMENTS -----------------------------------------------------------------------------
  adjustedData <- RunAdjustments(
    data = inputData$Table,
    adjustmentSpecs = adjustmentSpecs,
    diagYearRange = NULL,
    notifQuarterRange = NULL,
    seed = NULL
  )

  # 6. SAVE ADJUSTED DATA --------------------------------------------------------------------------
  # Last adjustment output is the final data
  finalData <- adjustedData[[length(adjustedData)]][["Table"]]

  hivModelData <- PrepareDataSetsForModel(
    finalData,
    splitBy = 'Imputation',
    strata = c('Gender', 'Transmission'),
    listIndex = 0
  )

  # Write output
  outputDataFilePath <- CreateOutputFileName(
    inputDataFilePath,
    suffix = paste0("_", GetTimeStamp())
  )
  WriteDataFile(finalData, outputDataFilePath)

  # 7. CREATE FINAL REPORT (OPTIONALLY) ------------------------------------------------------------
  reportFilePath <- GetReportFileNames()[reportName]
  params <- list(
    AdjustedData = adjustedData,
    ReportingDelay = TRUE,
    Smoothing = TRUE,
    CD4ConfInt = FALSE
  )

  if (is.element(reportName, c("Main Report"))) {
    params <- GetMainReportArtifacts(params)
  }

  params <- modifyList(
    params,
    list(
      Artifacts = list(
        FileName = inputDataFilePath,
        DiagYearRange = diagYearRange,
        NotifQuarterRange = notifQuarterRange,
        DiagYearRangeApply = TRUE
      )
    )
  )

  htmlReportFileName <- RenderReportToFile(
    reportFilePath = reportFilePath,
    format = "html_document",
    params = params,
    outDir = dirname(inputDataFilePath)
  )
  browseURL(htmlReportFileName)

  latexReportFileName <- RenderReportToFile(
    reportFilePath = reportFilePath,
    format = "latex_document",
    params = params,
    outDir = dirname(inputDataFilePath)
  )

  pdfReportFileName <- RenderReportToFile(
    reportFilePath = reportFilePath,
    format = "pdf_document",
    params = params,
    outDir = dirname(inputDataFilePath)
  )
  browseURL(pdfReportFileName)

  wordReportFileName <- RenderReportToFile(
    reportFilePath = reportFilePath,
    format = "word_document",
    params = params,
    outDir = dirname(inputDataFilePath)
  )
  browseURL(wordReportFileName)
}
