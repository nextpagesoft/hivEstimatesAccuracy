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
    uiOutput(ns("intermReport")),
    uiOutput(ns("runLog"))
  )
}

# Server logic
dataAdjust <- function(input, output, session, inputData)
{
  # Get namespace
  ns <- session$ns

  # Store reactive values
  adjustedData <- reactiveVal(NULL)
  vals <- reactiveValues(runLog = "",
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

    # Get widget html
    widget <- fluidRow(
      id = ns(rowId),
      column(1, actionLink(ns(deleteBtnId), "Remove"), style = "padding-top: 7px"),
      # Selection input
      column(5, selectInput(ns(adjustSelectId),
                            label = NULL,
                            choices = adjustmentSpecFileNames)),
      # Edit parameters button
      column(2, actionLink(ns(editParamBtnId), "Edit parameters"), style = "padding-top: 7px")
    )

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

    # Store for next adjustment selection widget addition
    vals$lastAdjustmentWidgetIndex <- index

    return(widget)
  }

  # Reactive values observers
  observeEvent(vals$adjustmentSpecs, {
    # finalData(NULL)
    adjustedData(NULL)
  })

  # Add adjustment selection widget
  observeEvent(input[["addAdjustBtn"]], {
    widget <- GetAdjustmentSelectionWidget()
    insertUI(selector = paste0("#", ns("adjustmentList")),
             where = "beforeEnd",
             ui = widget)
  })

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
      adjustedData(NULL)
      vals$runLog <- ""
      vals$intermReport <- ""
      startTime <- Sys.time()
      withProgress({
        tryCatch({
          runLog <- capture.output({
            adjustedData <- RunAdjustments(inputData,
                                           adjustmentSpecs = vals$adjustmentSpecs)

            interimReport <- ""
            for (i in seq_along(vals$adjustmentSpecs)) {
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

  # Output intermediate report when it has changed
  output[["intermReport"]] <- renderUI({
    if (vals$intermReport != "") {
      intermReportHTML <- box(
        class = "intermReport",
        width = 12,
        title = "Intermediate outputs of adjustments",
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

  return(adjustedData)
}
