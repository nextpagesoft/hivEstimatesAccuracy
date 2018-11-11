# Module globals
adjustmentSpecs <- lapply(GetAdjustmentSpecFileNames(),
                          GetListObject)

# Module globals
currYear <- year(Sys.time())

# Load application modules
modulesPath <- system.file("shiny/modules", package = "hivEstimatesAccuracy")
source(file.path(modulesPath, "inputDataUpload-Migrant.R"))

# User interface
inputDataUploadUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    div(
      id = ns("introductionBox"),
      box(
        width = 12,
        fluidRow(
          column(
            offset = 2,
            width = 8,
            style = "text-align: center; font-weight: bold",
            p("The ECDC HIV Estimates Accuracy Tool is an application that uses advanced statistical methods to correct for missing values in key HIV surveillance variables as well as for reporting delay."),
            p("The tool accepts case based HIV surveillance data prepared in a specific format."),
            p("The outputs include results from pre-defined analyses in the form of a report containing tables and graphs, and datasets, in which the adjustments have been incorporated and which may be exported for further analysis.",
              style = "margin-bottom: 0")
          )
        )
      )
    ),
    box(
      width = 12,
      title = "Input data",
      solidHeader = FALSE,
      status = "primary",
      collapsible = TRUE,
      fluidRow(
        column(width = 4,
               fileInput(ns("fileInput"),
                         width = "100%",
                         label = "File input:"),
               p("Maximum file size: 70MB",
                 tags$br(),
                 "Supported files types: rds, txt, csv, xls, xlsx (uncompressed and zip archives)")
        ),
        column(width = 8,
               withSpinner(uiOutput(ns("origDataDetails")),
                           type = 7,
                           proxy.height = "50px")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("attrMappingBox"),
        box(
          width = 12,
          title = "Attributes mapping",
          solidHeader = FALSE,
          status = "warning",
          collapsible = TRUE,
          p(
            HTML(
              paste("Please, provide mapping between attributes used internally by the tool (column \"Attribute\") and the input data dimensions (column \"Input data column\").",
                    "If \"Input data column\" is not specifid, then value in column \"Default value\" is used.",
                    sep = "<br />"
              )
            )
          ),
          actionButton(ns("applyMappingBtn"),
                       label = "Apply mapping",
                       style = "margin-bottom: 15px; background-color: #69b023; color: white"),
          fluidRow(
            column(8,
                   uiOutput(ns("attrMappingTableDiv"))),
            column(4,
                   uiOutput(ns("attrMappingInfoDiv")),
                   uiOutput(ns("attrMappingStatusBox")),
                   uiOutput(ns("valueCheckStatusBox"))
            )
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("migrantModule"),
        inputDataUploadMigrantUI(ns("migrant"))
      )
    ),
    uiOutput(ns("inputDataTableBox"))
  )
}

# Server logic
inputDataUpload <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  inputDataBeforeGrouping <- reactiveVal(NULL)

  # EVENT: Input data file name changed
  observeEvent(input[["fileInput"]], {
    # Get reference to file input
    fileInput <- input$fileInput

    # Validate
    validate(need(fileInput, message = FALSE))

    # Read input data
    uploadedData <- req(ReadDataFile(fileInput$datapath))
    stateFile <- GetIsState(uploadedData)
    appStatus$InputUploading <- TRUE

    if (stateFile) {
      originalData         <- req(uploadedData$OriginalData)
      originalDataAttrs    <- uploadedData$OriginalDataAttrs
      defaultValues        <- uploadedData$DefaultValues
      attrMapping          <- uploadedData$AttrMapping
      attrMappingStatus    <- uploadedData$AttrMappingStatus
      attrMappingValid     <- uploadedData$AttrMappingValid
      inputDataTest        <- uploadedData$InputDataTest
      inputDataTestStatus  <- uploadedData$InputDataTestStatus
      inputData            <- uploadedData$InputData
      adjustedData         <- uploadedData$AdjustedData
      adjustmentSpecs      <- uploadedData$AdjustmentSpecs
      miAdjustmentName     <- uploadedData$MIAdjustmentName
      rdAdjustmentName     <- uploadedData$RDAdjustmentName
      yearRange            <- uploadedData$YearRange
      yearRangeApply       <- uploadedData$YearRangeApply
      runLog               <- uploadedData$RunLog
      intermReport         <- uploadedData$IntermReport
      report               <- uploadedData$Report
      appStatus$StateUploading  <- TRUE
    } else {
      originalData      <- req(uploadedData)
      originalDataAttrs <- colnames(originalData)
      defaultValues     <- GetPreliminaryDefaultValues()
      attrMapping       <- GetPreliminaryAttributesMapping(originalData)

      # Apply a specific column mapping for column "FirstCD4Count" column
      if ("FirstCD4Count" %in% names(attrMapping) &&
          "cd4_num" %in% tolower(originalDataAttrs)) {
        attrMapping[["FirstCD4Count"]] <-
          originalDataAttrs[tolower(originalDataAttrs) == "cd4_num"][1]
      }

      attrMappingStatus    <- NULL
      attrMappingValid     <- FALSE
      inputDataTest        <- NULL
      inputDataTestStatus  <- NULL
      inputData            <- NULL
      adjustedData         <- NULL
      adjustmentSpecs      <- adjustmentSpecs
      miAdjustmentName     <- "None"
      rdAdjustmentName     <- "None"
      yearRange            <- c(1980, 2025)
      yearRangeApply       <- FALSE
      runLog               <- ""
      intermReport         <- ""
      report               <- ""
      appStatus$StateUploading  <- FALSE
    }

    appStatus$FileName            <- input$fileInput$name
    appStatus$OriginalData        <- originalData
    appStatus$OriginalDataAttrs   <- originalDataAttrs
    appStatus$DefaultValues       <- defaultValues
    appStatus$AttrMapping         <- attrMapping
    appStatus$AttrMappingStatus   <- attrMappingStatus
    appStatus$AttrMappingValid    <- attrMappingValid
    appStatus$InputDataTest       <- inputDataTest
    appStatus$InputDataTestStatus <- inputDataTestStatus
    appStatus$InputData           <- inputData
    appStatus$AdjustedData        <- adjustedData
    appStatus$AdjustmentSpecs     <- adjustmentSpecs
    appStatus$MIAdjustmentName    <- miAdjustmentName
    appStatus$RDAdjustmentName    <- rdAdjustmentName
    appStatus$YearRange           <- yearRange
    appStatus$YearRangeApply      <- yearRangeApply
    appStatus$RunLog              <- runLog
    appStatus$IntermReport        <- intermReport
    appStatus$Report              <- report
    inputDataBeforeGrouping(NULL)
  }, ignoreNULL = TRUE)

  # Show attributes mapping section
  observe({
    originalData <- appStatus$OriginalData
    if (is.null(originalData)) {
      shinyjs::hide("attrMappingBox", anim = TRUE, animType = "fade")
      shinyjs::hide("migrantModule", anim = TRUE, animType = "fade")
    } else {
      shinyjs::show("attrMappingBox")
      shinyjs::show("migrantModule")
    }
  })

  # Output original data details when they have changed
  output[["origDataDetails"]] <- renderUI({
    # Respond to originalData change
    originalData <- req(appStatus$OriginalData)

    isolate({
      # Get reference to file input
      fileInput <- input$fileInput

      # Get input data details
      origDataDetails <-
        fluidRow(
          column(3,
                 p(strong("Name:"), fileInput$name),
                 p(strong("Size:"), FormatObjectSize(fileInput$size)),
                 p(strong("Type:"), fileInput$type),
                 p(strong("Number of records:"), nrow(originalData))),
          column(9,
                 p(strong("Column names:")),
                 p(id = ns("colNamesP"),
                   paste(colnames(originalData), collapse = ", ")))
        )

      return(origDataDetails)
    })
  })

  # Generate attributes mapping table
  output[["attrMappingTableDiv"]] <- renderUI({
    # Respond to originalData change
    req(appStatus$OriginalData)

    isolate({

      ns <- session$ns

      # 1) Define rows
      attrMappingRows <- lapply(names(appStatus$AttrMapping), function(analysisAttr) {
        inputAttr <- appStatus$AttrMapping[[analysisAttr]]

        selectOptions <- list()
        if (is.null(inputAttr)) {
          selectOptions <- utils::modifyList(selectOptions,
                                             list(onInitialize = I('function() { this.setValue(""); }')))
        }

        selectId <- paste0("attr_", analysisAttr)
        defValInputId <- paste0("defVal_", analysisAttr)

        widget <- tags$tr(
          # Selection attribute
          tags$td(analysisAttr),
          # Selection data dimension
          tags$td(selectizeInput(ns(selectId),
                                 label = NULL,
                                 choices = appStatus$OriginalDataAttrs,
                                 selected = inputAttr,
                                 options = selectOptions)),
          tags$td(shinyjs::disabled(textInput(ns(defValInputId), NULL)))
        )

        # EVENT: Selected input column changed
        observeEvent(input[[selectId]], {
          oldMappedAttr <- appStatus$AttrMapping[[analysisAttr]]
          newMappedAttr <- input[[selectId]]
          if (!appStatus$StateUploading) {
            appStatus$AttrMapping[[analysisAttr]] <- newMappedAttr
            appStatus$AttrMappingStatus   <- NULL
            appStatus$InputDataTestStatus <- NULL
            appStatus$InputDataTest       <- NULL
            inputDataBeforeGrouping(NULL)
          }

          if (!IsEmptyString(newMappedAttr)) {
            shinyjs::disable(defValInputId)
          } else {
            shinyjs::enable(defValInputId)
          }
        })

        # EVENT: Default value changed
        observeEvent(input[[defValInputId]], {
          oldDefVal <- appStatus$DefaultValues[[analysisAttr]]
          newDefVal <- input[[defValInputId]]

          if (!appStatus$StateUploading) {
            appStatus$DefaultValues[[analysisAttr]] <- newDefVal
            appStatus$InputDataTestStatus <- NULL
            appStatus$InputDataTest       <- NULL
            inputDataBeforeGrouping(NULL)
          }
        })

        return(widget)
      })

      # 2) Define table
      attrMappingTableDiv <- tags$table(
        id = ns("attrMappingTable"),
        tags$thead(
          tags$tr(
            tags$th("Attribute", class = ns("attrNameCol")),
            tags$th("Input data column"),
            tags$th("Default value")
          )
        ),
        tags$tbody(
          attrMappingRows
        )
      )

      return(attrMappingTableDiv)
    })
  })

  output[["attrMappingInfoDiv"]] <- renderUI({

    valid <- appStatus$AttrMappingValid

    isolate({
      text <- NULL
      if (valid) {
        text <- span(icon("check"),
                     "Applied mapping is valid. You can proceeed to \"Migrant variable regrouping\" section below or other tabs.",
                     style = "color: seagreen")
      } else {
        text <- span("Input data has to be mapped to internal attributes and validated. Adjust mapping and press \"Apply mapping\" button to the left.")
      }

      attrMappingInfoBox <- box(width = 12,
                                style = "background-color: ghostwhite;",
                                text)

      return(attrMappingInfoBox)
    })
  })

  # Respond to "Apply mapping" button click
  observeEvent(input[["applyMappingBtn"]], {
    withProgress(message = "Attributes mapping",
                 value = 0, {
                   originalData <- appStatus$OriginalData
                   setProgress(0.1, detail = "Applying mapping")

                   inputDataTest <- ApplyAttributesMapping(originalData,
                                                           appStatus$AttrMapping,
                                                           appStatus$DefaultValues)
                   setProgress(0.4, detail = "Pre-processing data with a single imputation of Gender")

                   inputDataTest <- PreProcessInputData(inputDataTest)
                   setProgress(0.9, detail = "Checking data validity")

                   appStatus$AttrMappingStatus <- GetAttrMappingStatus(appStatus$AttrMapping)
                   appStatus$InputDataTestStatus <- GetInputDataValidityStatus(inputDataTest$Table)
                   appStatus$InputDataTest <- inputDataTest
                   appStatus$InputUploading <- FALSE
                   appStatus$StateUploading <- FALSE

                   setProgress(1, detail = "Done")
                 })
  })

  # Populate attribute mapping status UI
  output[["attrMappingStatusBox"]] <- renderUI({
    attrMappingStatus <- req(appStatus$AttrMappingStatus)

    isolate({
      multipleMappedDataAttrs <- attrMappingStatus$MultipleMapped

      header <- NULL
      textColor <- NULL
      if (attrMappingStatus$Valid) {
        textColor <- "seagreen"
        headerHTML <- span(icon("check"),
                           "Assignment of input data columns to attributes is valid.",
                           style = paste("color:", textColor))
      } else {
        textColor <- "red"
        headerHTML <- span(icon("exclamation"),
                           "Assignment of input data columns to attributes is not valid.",
                           style = paste("color:", textColor))
      }

      multiMappedStatusHTML <- NULL
      if (length(multipleMappedDataAttrs) > 0) {
        multiMappedStatusHTML <- tagList(
          tags$ol(
            lapply(names(multipleMappedDataAttrs), function(dataAttr) {
              analysisAttrs <- multipleMappedDataAttrs[[dataAttr]]
              tags$li(sprintf("Input data column %s is mapped to attributes %s.",
                              AddQuoteMarks(dataAttr),
                              paste(AddQuoteMarks(analysisAttrs), collapse = ", ")))
            })
          )
        )
      }

      attrMappingStatusBox <- box(width = 12,
                                  style = "background-color: ghostwhite;",
                                  headerHTML,
                                  multiMappedStatusHTML)

      return(attrMappingStatusBox)
    })
  })

  output[["valueCheckStatusBox"]] <- renderUI({
    # Respond to InputDataTestStatus change
    inputDataTestStatus <- req(appStatus$InputDataTestStatus)

    isolate({
      wrongValuesCols <- Filter(function(x) length(x$WrongValues) != 0,
                                inputDataTestStatus$CheckStatus)

      headerHTML <- NULL
      textColor <- NULL
      if (inputDataTestStatus$Valid) {
        textColor <- "seagreen"
        headerHTML <- span(icon("check"),
                           "Input data values are valid.",
                           style = paste("color:", textColor))
      } else {
        textColor <- "red"
        headerHTML <- span(icon("exclamation"),
                           HTML("Input data values are not valid."),
                           style = paste("color:", textColor))
      }

      wrongValuesHTML <- NULL
      if (length(wrongValuesCols) > 0) {
        wrongValuesHTML <- tags$ol(
          lapply(names(wrongValuesCols), function(colName) {
            wrongValuesCol <- wrongValuesCols[[colName]]
            tags$li(sprintf("Attribute %s contains invalid value(s) %s.",
                            AddQuoteMarks(colName),
                            paste(AddQuoteMarks(wrongValuesCol$WrongValues), collapse = ", ")))
          })
        )
      }

      valueCheckStatusBox <- box(width = 12,
                                 style = "background-color: ghostwhite;",
                                 headerHTML,
                                 wrongValuesHTML)

      return(valueCheckStatusBox)
    })
  })

  observeEvent(c(appStatus$AttrMappingStatus$Valid,
                 appStatus$InputDataTestStatus$Valid), {
    if (is.null(appStatus$AttrMappingStatus) || is.null(appStatus$InputDataTestStatus)) {
      inputDataBeforeGrouping(NULL)
      appStatus$AttrMappingValid <- FALSE
    } else if (appStatus$AttrMappingStatus$Valid && appStatus$InputDataTestStatus$Valid) {
      inputDataBeforeGrouping(appStatus$InputDataTest)
      appStatus$AttrMappingValid <- TRUE
    } else {
      inputDataBeforeGrouping(NULL)
      appStatus$AttrMappingValid <- FALSE
    }
  })

  output[["inputDataTableBox"]] <- renderUI({
    if (appStatus$AttrMappingValid) {
      box(
        width = 12,
        title = "Input data records pre-processed",
        solidHeader = FALSE,
        status = "warning",
        collapsible = TRUE,
        dataTableOutput(ns("inputDataTable"))
      )
    }
  })

  output[["inputDataTable"]] <- renderDataTable(
    appStatus$InputData$Table[, -c("GroupOfOrigin", "SqCD4", "MinNotificationTime",
                                   "MaxNotificationTime", "VarX", "TweakedVarX",
                                   "MaxPossibleDelay", "TweakedMaxPossibleDelay")],
    options = list(
      dom = '<"top">lirt<"bottom">p',
      autoWidth = FALSE,
      pageLength = 15,
      scrollX = TRUE,
      deferRender = TRUE,
      serverSide = TRUE,
      scroller = FALSE))

  callModule(inputDataUploadMigrant, "migrant", appStatus, inputDataBeforeGrouping)

  return(NULL)
}
