# Store libPaths of the master process.
# They are set on the spawned R processes created by parallel.
# Otherwise, libraries installed in packrat folder cannot be found by those processes.
libPaths <- .libPaths()

# Module globals
adjustmentSpecFileNames <- GetAdjustmentSpecFileNames()

isLinux <- tolower(Sys.info()[["sysname"]]) == "linux"

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
      actionButton(ns("runAdjustBtn"), "Run"),
      shinyjs::disabled(actionButton(ns("cancelAdjustBtn"), "Cancel"))
    ),
    uiOutput(ns("intermReport")),
    uiOutput(ns("runLog"))
  )
}

# Server logic
dataAdjust <- function(input, output, session, inputData)
{
  # Get namespace
  ns <- session$ns

  # Make "task" behave like a reactive value
  makeReactiveBinding("task")
  task <- NULL

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

    # EVENT: Button "Remove" clicked
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
        easyClose = FALSE,
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
    inputData <- req(inputData())
    adjustmentSpecs <- req(vals$adjustmentSpecs)

    shinyjs::disable("runAdjustBtn")
    shinyjs::enable("cancelAdjustBtn")

    adjustedData(NULL)

    vals$runLog <- ""
    vals$intermReport <- ""

    # Show progress message during task start
    prog <- Progress$new(session)
    prog$set(message = "Running adjustments...", value = 0.1)

    startTime <- Sys.time()

    if (isLinux) {
      task <<- CreateTask({
        RunAdjustments(
          inputData$Table,
          adjustmentSpecs = adjustmentSpecs)
      })
    } else {
      task <<- CreateTask(function(x, y) {
        hivEstimatesAccuracy::RunAdjustments(
          data = x,
          adjustmentSpecs = y)
      },
      args = list(inputData$Table, adjustmentSpecs))
    }

    o <- observe({
      # Only proceed when the task is completed (this could mean success,
      # failure, or cancellation)
      req(task$completed())
      endTime <- Sys.time()

      adjustedData <- task$result()
      task <<- NULL
      if (is.list(adjustedData)) {
        adjustedData(adjustedData)

        intermReport <- RenderReportToHTML(
          reportFilePath = system.file("reports/intermediate/0.PreProcess.Rmd",
                                       package = "hivEstimatesAccuracy"),
          params = list(InputData = inputData))
        for (i in seq_along(adjustmentSpecs)) {
          intermReport <- paste(
            intermReport,
            RenderReportForAdjSpec(adjustmentSpecs[[i]],
                                   "intermediate",
                                   adjustedData[[i]]))
        }
        vals$intermReport <- HTML(intermReport)
        vals$runLog <- "Done"
      } else {
        adjustedData(NULL)
        vals$runLog <- "Adjustments cancelled"
      }
      vals$runLog <- paste(paste("Start time  :", FormatTime(startTime)),
                           paste("End time    :", FormatTime(endTime)),
                           paste("Elapsed time:", FormatDiffTime(endTime - startTime)),
                           paste(""),
                           vals$runLog,
                           sep = "\n")

      # This observer only runs once
      o$destroy()

      # Close the progress indicator and update button state
      prog$close()
      shinyjs::enable("runAdjustBtn")
      shinyjs::disable("cancelAdjustBtn")
    })

  })

  # EVENT: Button "Run adjustments" clicked
  observeEvent(input[["cancelAdjustBtn"]], {
    req(task)$cancel()
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
