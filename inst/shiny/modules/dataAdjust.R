# Store libPaths of the master process.
# They are set on the spawned R processes created by parallel.
# Otherwise, libraries installed in packrat folder cannot be found by those processes.
libPaths <- .libPaths()

# Module globals
adjustmentSpecFileNames <- GetAdjustmentSpecFileNames()

# User interface
dataAdjustUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "Run adjustments",
      solidHeader = FALSE,
      status = "primary",
      collapsible = TRUE,
      actionButton(ns("addAdjustBtn"), "Add"),
      p(""),
      div(id = ns("adjustmentList")),
      actionButton(ns("runAdjustBtn"), "Run")),
    uiOutput(ns("diagReport")),
    uiOutput(ns("intermReport")),
    uiOutput(ns("runLog")),
    uiOutput(ns("dataDetails"))
  )
}

# Server logic
dataAdjust <- function(input, output, session, inputData)
{
  # Get namespace
  ns <- session$ns

  # Store reactive values
  finalData <- reactiveVal(NULL)
  adjustedData <- reactiveVal(NULL)
  vals <- reactiveValues(runLog = "",
                         diagReport = "",
                         intermReport = "",
                         adjustmentSpecs = list(),
                         lastAdjustmentWidgetIndex = 0L,
                         editedAdjustmentKey = "",
                         editedAdjustmentParamsWidgets = list())

  # Get widget for selecting an adjustment
  GetAdjustmentSelectionWidget <- function() {
    # Get unique index for the elements
    index <- vals$lastAdjustmentWidgetIndex + 1
    key <- as.character(index)

    rowId          <- paste0("row", key)
    deleteBtnId    <- paste0("deleteBtn", key)
    adjustSelectId <- paste0("adjustSelect", key)
    editParamBtnId <- paste0("adjustParamBtn", key)
    runDiagBtnId   <- paste0("runDiagBtn", key)
    downloadCsvBtnId  <- paste0("downloadCsvBtn", key)
    downloadStataBtnId  <- paste0("downloadStataBtn", key)

    # Get widget html
    widget <- fluidRow(
      id = ns(rowId),
      column(1, actionLink(ns(deleteBtnId), "Remove"), style = "padding-top: 7px"),
      # Selection input
      column(5, selectInput(ns(adjustSelectId),
                            label = NULL,
                            choices = adjustmentSpecFileNames)),
      # Edit parameters button
      column(2, actionLink(ns(editParamBtnId), "Edit parameters"), style = "padding-top: 7px"),
      column(2, actionLink(ns(runDiagBtnId), "Run diagnostic"), style = "padding-top: 7px"),
      column(1, shinyjs::hidden(downloadLink(ns(downloadCsvBtnId), "Download csv")), style = "padding-top: 7px"),
      column(1, shinyjs::hidden(downloadLink(ns(downloadStataBtnId), "Download Stata")), style = "padding-top: 7px"))

    # EVENT: Adjustment selection changed
    observeEvent(input[[adjustSelectId]], {
      # Add selected adjustment to the list of adjustments to run
      selectedAdjustmentFileName <- input[[adjustSelectId]]
      if (file.exists(selectedAdjustmentFileName)) {
        # Get adjustment specification and enrich with key for later reference
        adjustmentSpec <- GetListObject(selectedAdjustmentFileName)
        adjustmentSpec$Key <- key
        vals$adjustmentSpecs[[key]] <- adjustmentSpec
      }
    })

    # EVENT: Button "Delete" clicked
    observeEvent(input[[deleteBtnId]], {
      vals$adjustmentSpecs[[key]] <- NULL
      removeUI(selector = paste0("#", ns(rowId)))
    })

    # EVENT: Button "Edit parameters" clicked
    observeEvent(input[[editParamBtnId]], {
      vals$editedAdjustmentKey <- key
    })

    # EVENT: Button "Run diagnostic" clicked
    observeEvent(input[[runDiagBtnId]], {
      inputData <- inputData()
      if (!is.null(inputData)) {
        withProgress(
          message = "Running diagnostic",
          detail = "The report will be displayed shortly.",
          value = 0, {
            # Define parameters
            adjustmentSpec <- vals$adjustmentSpecs[[key]]
            diagReportFileName <-
              system.file(file.path("reports/diagnostic",
                                    paste0(tools::file_path_sans_ext(basename(adjustmentSpec$FileName)),
                                           "_diagnostic.Rmd")),
                          package = "hivEstimatesAccuracy")

            parameters <- GetParamInfoFromAdjustSpec(adjustmentSpec$Parameters, infoType = "value")

            setProgress(0.1)

            diagHtmlFileName <- RenderReportToFile(filePath = diagReportFileName,
                                                   format = "html_fragment",
                                                   params = list(InputData = list(Table = inputData,
                                                                                  Parameters = parameters)))

            setProgress(0.8)

            diagReportFileContent <- ReadStringFromFile(diagHtmlFileName)
            setProgress(0.9)

            file.remove(diagHtmlFileName)
            diagReport <- HTML(diagReportFileContent)
            setProgress(1)
          }
        )

        vals$diagReport <- diagReport
      }
    })

    # Store for next adjustment selection widget addition
    vals$lastAdjustmentWidgetIndex <- index

    return(widget)
  }

  # Reactive values observers
  observeEvent(vals$adjustmentSpecs, {
    finalData(NULL)
    adjustedData(NULL)
  })

  # Add adjustment selection widget
  observeEvent(input[["addAdjustBtn"]], {
    widget <- GetAdjustmentSelectionWidget()
    insertUI(selector = paste0("#", ns("adjustmentList")),
             where = "beforeEnd",
             ui = widget)
  })

  # EVENT: Button "Download csv/stata data" clicked
  DownloadIntermediateData <- function(key, format) {
    adjustedData <- adjustedData()
    if (key %in% names(adjustedData)) {
      intermediateData <- adjustedData[[key]]$Table
    } else {
      intermediateData <- NULL
    }

    downloadHandler(
      filename = function() {
        fileName <- paste("data", format, sep = ".")
        return(fileName)
      },
      content = function(file) {
        withProgress(message = "Creating intermediate data output file",
                     detail = "The file will be available for download shortly.",
                     value = 0, {
                       setProgress(0)
                       WriteDataFile(intermediateData, file)
                       setProgress(1)
                     })
        return(NULL)
      })
  }

  EnableIntermediateDataDownloadLinks <- function() {
    for (adjustmentSpec in vals$adjustmentSpecs) {
      key <- adjustmentSpec$Key
      downloadCsvBtnId  <- paste0("downloadCsvBtn", key)
      downloadStataBtnId  <- paste0("downloadStataBtn", key)

      # Define download handler
      output[[downloadCsvBtnId]] <- DownloadIntermediateData(key, "csv")
      output[[downloadStataBtnId]] <- DownloadIntermediateData(key, "dta")
      # Show download link
      shinyjs::show(downloadCsvBtnId)
      shinyjs::show(downloadStataBtnId)
    }
  }

  # Populate adjustment parameter widgets in the editing dialog
  output[["adjustmentList"]] <- renderUI({
    vals$adjustmentSelectionWidgets
  })

  # Get widgets for editing parameters in a dialog
  GetAdjustParamsWidgets <- function(adjustmentSpec) {
    # Reformat slightly - add "name" property for "GetParamWidgets"
    paramSpecs <- adjustmentSpec$Parameter
    paramSpecs <- setNames(lapply(names(paramSpecs), function(name) {
      paramSpec <- paramSpecs[[name]]
      paramSpec$name <- name
      structure(paramSpec, class = "knit_param")
    }), names(paramSpecs))

    widgets <- GetParamWidgets(paramSpecs, ns = ns)

    return(widgets)
  }

  # Show adjustment parameters editing dialog
  observeEvent(vals$editedAdjustmentKey, {
    if (vals$editedAdjustmentKey != "") {
      editedAdjustmentSpec <- vals$adjustmentSpecs[[vals$editedAdjustmentKey]]
      vals$editedAdjustmentParamsWidgets <- GetAdjustParamsWidgets(editedAdjustmentSpec)

      showModal(modalDialog(
        title = "Edit adjustment parameters",
        uiOutput(ns("adjustmentParams")),
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("paramsDlgCancel"), "Cancel"),
          actionButton(ns("paramsDlgOk"), "OK")
        )
      ))
    } else {
      vals$editedAdjustmentParamsWidgets <- list()
    }
  })

  # Populate adjustment parameter widgets in the editing dialog
  output[["adjustmentParams"]] <- renderUI({
    vals$editedAdjustmentParamsWidgets
  })

  # Adjustment parameters editing dialog CLOSE through OK event
  observeEvent(input[["paramsDlgOk"]], {
    # Copy parameters from dialog
    adjustmentParams <- vals$adjustmentSpecs[[vals$editedAdjustmentKey]]$Parameters
    for (paramName in names(vals$editedAdjustmentParamsWidgets)) {
      adjustmentParams[[paramName]]$value <- input[[paramName]]
    }
    # Save parameters
    vals$adjustmentSpecs[[vals$editedAdjustmentKey]]$Parameters <- adjustmentParams

    # Clean up
    vals$editedAdjustmentKey <- ""
    vals$editedAdjustmentParamsWidgets <- list()
    removeModal()
  })

  # Adjustment parameters editing dialog CLOSE through Cancel event
  observeEvent(input[["paramsDlgCancel"]], {
    # Clean up
    vals$editedAdjustmentKey <- ""
    vals$editedAdjustmentParamsWidgets <- list()
    removeModal()
  })

  # EVENT: Button "Run adjustments" clicked
  observeEvent(input[["runAdjustBtn"]], {
    inputData <- inputData()
    if (!is.null(inputData)) {
      # Run adjustments
      finalData(NULL)
      adjustedData(NULL)
      vals$runLog <- ""
      vals$intermReport <- ""
      vals$diagReport <- ""
      startTime <- Sys.time()
      withProgress({
        tryCatch({
          runLog <- capture.output({
            adjustedData <- RunAdjustments(inputData,
                                           adjustmentSpecs = vals$adjustmentSpecs)

            interimReport <- ""
            for (i in seq_len(length(vals$adjustmentSpecs))) {
              interimReport <- paste(interimReport,
                                     RenderReportForAdjSpec(vals$adjustmentSpecs[[i]],
                                                            "intermediate",
                                                            adjustedData[[i]]))
            }
            vals$intermReport <- HTML(interimReport)
          })
          vals$runLog <- paste(runLog, collapse = "\n")
        }, error = function(err) {
          print(err)
          adjustedData <- NULL
        }, finally = {
          adjustedData(adjustedData)
        })
      },
      message = "Running adjustments...",
      value = 0.1)
      endTime <- Sys.time()
      vals$runLog <- paste(paste("Start time  :", FormatTime(startTime)),
                           paste("End time    :", FormatTime(endTime)),
                           paste("Elapsed time:", FormatDiffTime(endTime - startTime)),
                           paste(""),
                           vals$runLog,
                           sep = "\n")
    }
  })

  # Output report when it has changed
  output[["diagReport"]] <- renderUI({
    if (vals$diagReport != "") {
      diagReportHTML <- box(
        width = 12,
        title = "Diagnostic report",
        solidHeader = FALSE,
        collapsible = TRUE,
        status = "warning",
        vals$diagReport
      )
    } else {
      diagReportHTML <- NULL
    }
    return(diagReportHTML)
  })

  # Output intermediate report when it has changed
  output[["intermReport"]] <- renderUI({
    if (vals$intermReport != "") {
      intermReportHTML <- box(
        class = "intermReport",
        width = 12,
        title = "Adjustments outputs report",
        solidHeader = FALSE,
        collapsible = TRUE,
        status = "warning",
        vals$intermReport
      )
    } else {
      intermReportHTML <- NULL
    }
    return(intermReportHTML)
  })

  # Populate adjustment parameter widgets in the editing dialog
  output[["runLog"]] <- renderUI({
    if (vals$runLog != "") {
      runLogHTML <- box(
        width = 12,
        title = "Run log",
        solidHeader = FALSE,
        collapsible = TRUE,
        status = "warning",
        tags$pre(vals$runLog)
      )
    } else {
      runLogHTML <- NULL
    }
    return(runLogHTML)
  })

  # Observe change in adjustedData object
  observeEvent(adjustedData(), {
    adjustedData <- adjustedData()
    if (length(adjustedData) > 0) {
      finalData(adjustedData[[length(adjustedData)]])
      EnableIntermediateDataDownloadLinks()
    } else {
      finalData(NULL)
    }
  })

  # Output adjusted data details when they have changed
  output[["dataDetails"]] <- renderUI({
    # Respond to adjustedData change
    finalData <- finalData()$Table

    isolate({
      if (!is.null(finalData)) {
        # Get adjusted data details
        finalDataDetailsHTML <-
          box(
            width = 12,
            title = "Adjusted data details",
            solidHeader = FALSE,
            collapsible = TRUE,
            status = "warning",
            div(
              p(strong("Number of records:"), nrow(finalData)),
              p(strong("Column names:"), paste(names(finalData), collapse = ", ")))
          )
      } else {
        finalDataDetailsHTML <- NULL
      }

      return(finalDataDetailsHTML)
    })
  })

  return(finalData)
}
