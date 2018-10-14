# Store libPaths of the master process.
# They are set on the spawned R processes created by parallel.
# Otherwise, libraries installed in packrat folder cannot be found by those processes.
libPaths <- .libPaths()

# Module globals
adjustmentSpecs <- lapply(GetAdjustmentSpecFileNames(),
                          GetListObject)
adjustmentSpecsType <- lapply(adjustmentSpecs, "[[", "Type")

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
      fluidRow(
        column(3, "1. Multiple Imputations adjustment", style = "padding-top: 7px"),
        column(4, selectInput(
          ns("miSelect"),
          label = NULL,
          choices = c(names(adjustmentSpecs[adjustmentSpecsType == "MULTIPLE_IMPUTATIONS"]),
                      "None"),
          selected = "None")
        ),
        column(2, actionLink(ns("miSelectParam"), "Edit parameters"), style = "padding-top: 7px")
      ),
      fluidRow(
        column(3, "2. Reporting Delays adjustment", style = "padding-top: 7px"),
        column(4, selectInput(
          ns("rdSelect"),
          label = NULL,
          choices = c(names(adjustmentSpecs[adjustmentSpecsType == "REPORTING_DELAYS"]),
                      "None"),
          selected = "None")
        ),
        column(2, actionLink(ns("rdSelectParam"), "Edit parameters"), style = "padding-top: 7px")
      ),
      actionButton(ns("runAdjustBtn"), "Run"),
      shinyjs::disabled(actionButton(ns("cancelAdjustBtn"), "Cancel"))
    ),
    uiOutput(ns("intermReport")),
    uiOutput(ns("runLog"))
  )
}

# Server logic
dataAdjust <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  # Make "task" behave like a reactive value
  makeReactiveBinding("task")
  task <- NULL

  # Store reactive values
  #adjustedData <- reactiveVal(NULL)
  vals <- reactiveValues(runLog = "",
                         intermReport = "",
                         adjustmentSpecs = adjustmentSpecs,
                         miAdjustmentName = "None",
                         rdAdjustmentName = "None",
                         editedAdjustmentName = "None",
                         editedAdjustmentParamsWidgets = list())

  # EVENT: MI adjustment selection changed
  observeEvent(input[["miSelect"]], {
    adjustmentName <- input[["miSelect"]]
    vals$miAdjustmentName <- adjustmentName
    if (adjustmentName != "None") {
      shinyjs::show("miSelectParam")
    } else {
      shinyjs::hide("miSelectParam")
    }
    appStatus$AdjustedData <- NULL
  })

  # EVENT: RD adjustment selection changed
  observeEvent(input[["rdSelect"]], {
    adjustmentName <- input[["rdSelect"]]
    vals$rdAdjustmentName <- adjustmentName
    if (adjustmentName != "None") {
      shinyjs::show("rdSelectParam")
    } else {
      shinyjs::hide("rdSelectParam")
    }
    appStatus$AdjustedData <- NULL
  })

  # EVENT: Button "Edit parameters" clicked
  observeEvent(input[["miSelectParam"]], {
    vals$editedAdjustmentName <- vals$miAdjustmentName
  })

  # EVENT: Button "Edit parameters" clicked
  observeEvent(input[["rdSelectParam"]], {
    vals$editedAdjustmentName <- vals$rdAdjustmentName
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
  observeEvent(vals$editedAdjustmentName, {
    if (vals$editedAdjustmentName != "None") {
      editedAdjustmentSpec <- vals$adjustmentSpecs[[vals$editedAdjustmentName]]
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
    adjustmentParams <- vals$adjustmentSpecs[[vals$editedAdjustmentName]]$Parameters
    for (paramName in names(vals$editedAdjustmentParamsWidgets)) {
      adjustmentParams[[paramName]]$value <- input[[paramName]]
    }
    # Save parameters in the selected adjustment object
    vals$adjustmentSpecs[[vals$editedAdjustmentName]]$Parameters <- adjustmentParams

    # Clean up
    vals$editedAdjustmentName <- "None"
    vals$editedAdjustmentParamsWidgets <- list()
    removeModal()
  })

  # Adjustment parameters editing dialog CLOSE through Cancel event
  observeEvent(input[["paramsDlgCancel"]], {
    # Clean up
    vals$editedAdjustmentName <- "None"
    vals$editedAdjustmentParamsWidgets <- list()
    removeModal()
  })

  adjustmentsValid <- reactive({
    vals$miAdjustmentName != "None" || vals$rdAdjustmentName != "None"
  })

  observe({
    if (adjustmentsValid()) {
      shinyjs::enable("runAdjustBtn")
    } else {
      shinyjs::disable("runAdjustBtn")
    }
  })

  # EVENT: Button "Run adjustments" clicked
  observeEvent(input[["runAdjustBtn"]], {
    inputData <- req(appStatus$InputData)
    adjustmentSpecs <- list()
    if (vals$miAdjustmentName != "None") {
      adjustmentSpecs[[vals$miAdjustmentName]] <- vals$adjustmentSpecs[[vals$miAdjustmentName]]
    }
    if (vals$rdAdjustmentName != "None") {
      adjustmentSpecs[[vals$rdAdjustmentName]] <- vals$adjustmentSpecs[[vals$rdAdjustmentName]]
    }

    shinyjs::disable("runAdjustBtn")
    shinyjs::enable("cancelAdjustBtn")

    appStatus$AdjustedData <- NULL

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
        appStatus$AdjustedData <- adjustedData

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

  return(NULL)
}
