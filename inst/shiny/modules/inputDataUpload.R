# Module globals
adjustmentSpecs <- lapply(GetAdjustmentSpecFileNames(), GetListObject)

# Load application modules
modulesPath <- system.file('shiny/modules/inputDataUpload', package = 'hivEstimatesAccuracy')
source(file.path(modulesPath, 'caseBasedMainWidget.R'))

# User interface
inputDataUploadUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = 'margin-top: 15px'),
    box(
      width = 12,
      title = 'Input data upload',
      solidHeader = FALSE,
      status = 'primary',
      collapsible = FALSE,
      tabsetPanel(
        id = 'tabs',
        type = 'tabs',
        tabPanel(
          'Case-based data upload',
          fluidRow(
            style = 'margin-top: 10px',
            column(
              width = 4,
              fileInput(ns('caseFileInput'), width = '100%', label = 'File input:'),
              p(
                'Maximum file size: 70MB', tags$br(),
                'Supported files types: rds, txt, csv, xls, xlsx (uncompressed and zip archives)'
              )
            ),
            column(
              width = 8,
              withSpinner(uiOutput(ns('caseDataDetails')), type = 7, proxy.height = '50px')
            )
          ),
          fluidRow(caseBasedMainWidgetUI(ns('main')))
        ),
        tabPanel(
          'Aggregated data upload',
          fluidRow(
            style = 'margin-top: 10px',
            column(
              width = 4,
              wellPanel('Upload widget')
            ),
            column(
              width = 8,
              wellPanel('Details widget')
            )
          )
        )
      )
    )
  )
}

# Server logic
inputDataUpload <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  # EVENT: Input data file name changed
  observeEvent(input[['caseFileInput']], {
    # Get reference to file input
    fileInput <- input$caseFileInput

    # Validate
    validate(need(fileInput, message = FALSE))

    # Read input data
    uploadedData <- req(ReadDataFile(fileInput$datapath))
    stateFile <- GetIsState(uploadedData)
    appStatus$InputUploading <- TRUE

    if (stateFile) {
      originalData           <- req(uploadedData$OriginalData)
      originalDataAttrs      <- uploadedData$OriginalDataAttrs
      defaultValues          <- uploadedData$DefaultValues
      seed                   <- uploadedData$Seed
      attrMapping            <- uploadedData$AttrMapping
      attrMappingStatus      <- uploadedData$AttrMappingStatus
      attrMappingValid       <- uploadedData$AttrMappingValid
      inputDataTest          <- uploadedData$InputDataTest
      inputDataTestStatus    <- uploadedData$InputDataTestStatus
      inputData              <- uploadedData$InputData
      adjustedData           <- uploadedData$AdjustedData
      hivModelData           <- uploadedData$HIVModelData
      adjustmentSpecs        <- uploadedData$AdjustmentSpecs
      miAdjustmentName       <- uploadedData$MIAdjustmentName
      rdAdjustmentName       <- uploadedData$RDAdjustmentName
      diagYearRange          <- uploadedData$DiagYearRange
      diagYearRangeApply     <- uploadedData$DiagYearRangeApply
      notifQuarterRange      <- uploadedData$NotifQuarterRange
      notifQuarterRangeApply <- uploadedData$NotifQuarterRangeApply
      runLog                 <- uploadedData$RunLog
      intermReport           <- uploadedData$IntermReport
      report                 <- uploadedData$Report
      appStatus$StateUploading  <- TRUE
    } else {
      originalData      <- req(uploadedData)
      originalDataAttrs <- colnames(originalData)
      defaultValues     <- GetPreliminaryDefaultValues()
      attrMapping       <- GetPreliminaryAttributesMapping(originalData)

      # Apply a specific column mapping for column 'FirstCD4Count' column
      if ('FirstCD4Count' %in% names(attrMapping) &&
          'cd4_num' %in% tolower(originalDataAttrs)) {
        attrMapping[['FirstCD4Count']] <-
          originalDataAttrs[tolower(originalDataAttrs) == 'cd4_num'][1]
      }

      seed                   <- NULL
      attrMappingStatus      <- NULL
      attrMappingValid       <- FALSE
      inputDataTest          <- NULL
      inputDataTestStatus    <- NULL
      inputData              <- NULL
      adjustedData           <- NULL
      hivModelData           <- NULL
      adjustmentSpecs        <- adjustmentSpecs
      miAdjustmentName       <- 'None'
      rdAdjustmentName       <- 'None'
      diagYearRange          <- c(1980, 2025)
      diagYearRangeApply     <- FALSE
      notifQuarterRange      <- c(1980, 2025)
      notifQuarterRangeApply <- FALSE
      runLog                 <- ''
      intermReport           <- ''
      report                 <- ''
      appStatus$StateUploading  <- FALSE
    }

    appStatus$Seed                    <- seed
    appStatus$FileName                <- input$fileInput$name
    appStatus$OriginalData            <- originalData
    appStatus$OriginalDataAttrs       <- originalDataAttrs
    appStatus$DefaultValues           <- defaultValues
    appStatus$AttrMapping             <- attrMapping
    appStatus$AttrMappingStatus       <- attrMappingStatus
    appStatus$AttrMappingValid        <- attrMappingValid
    appStatus$InputDataTest           <- inputDataTest
    appStatus$InputDataTestStatus     <- inputDataTestStatus
    appStatus$InputDataBeforeGrouping <- NULL
    appStatus$InputData               <- inputData
    appStatus$AdjustedData            <- adjustedData
    appStatus$HIVModelData            <- hivModelData
    appStatus$AdjustmentSpecs         <- adjustmentSpecs
    appStatus$MIAdjustmentName        <- miAdjustmentName
    appStatus$RDAdjustmentName        <- rdAdjustmentName
    appStatus$DiagYearRange           <- diagYearRange
    appStatus$DiagYearRangeApply      <- diagYearRangeApply
    appStatus$NotifQuarterRange       <- notifQuarterRange
    appStatus$NotifQuarterRangeApply  <- notifQuarterRangeApply
    appStatus$RunLog                  <- runLog
    appStatus$IntermReport            <- intermReport
    appStatus$Report                  <- report
  }, ignoreNULL = TRUE)

  # Output case-based data details when they have changed
  output[['caseDataDetails']] <- renderUI({
    # Respond to originalData change
    originalData <- req(appStatus$OriginalData)

    isolate({
      # Get reference to file input
      fileInput <- input$caseFileInput

      # Get input data details
      origDataDetails <-
        fluidRow(
          column(
            3,
            p(strong('Name:'), fileInput$name),
            p(strong('Size:'), FormatObjectSize(fileInput$size)),
            p(strong('Type:'), fileInput$type),
            p(strong('Number of records:'), nrow(originalData))),
          column(
            9,
            p(strong('Column names:')),
            p(id = ns('colNamesP'), paste(colnames(originalData), collapse = ', '))
          )
        )

      return(origDataDetails)
    })
  })

  callModule(caseBasedMainWidget, 'main', appStatus)

  return(NULL)
}
