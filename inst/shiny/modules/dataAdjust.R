# Store libPaths of the master process.
# They are set on the spawned R processes created by parallel.
# Otherwise, libraries installed in packrat folder cannot be found by those processes.
# libPaths <- .libPaths()

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
      uiOutput(ns("rerunInfo")),
      actionButton(ns("runAdjustBtn"), "Run", style = "background-color: #69b023; color: white"),
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
  vals <- reactiveValues(editedAdjustmentName = "None",
                         editedAdjustmentParamsWidgets = list())

  invalidateAdjustments <- function() {
    appStatus$AdjustedData <- NULL
  }

  observeEvent(appStatus$AdjustedData, {
    if (is.null(appStatus$AdjustedData)) {
      appStatus$RunLog <- ""
      appStatus$IntermReport <- ""
      appStatus$Report <- NULL
    }
  }, ignoreNULL = FALSE)

  observeEvent(appStatus$InputUploading, {
    updateSelectInput(session,
                      "rdSelect",
                      selected = appStatus$RDAdjustmentName)
    updateSelectInput(session,
                      "miSelect",
                      selected = appStatus$MIAdjustmentName)
  })

  # EVENT: MI adjustment selection changed
  observeEvent(input[["miSelect"]], {
    adjustmentName <- input[["miSelect"]]
    appStatus$MIAdjustmentName <- adjustmentName
    if (adjustmentName != "None") {
      shinyjs::show("miSelectParam")
    } else {
      shinyjs::hide("miSelectParam")
    }
    if (!appStatus$StateUploading) {
      invalidateAdjustments()
    }
  })

  # EVENT: RD adjustment selection changed
  observeEvent(input[["rdSelect"]], {
    adjustmentName <- input[["rdSelect"]]
    appStatus$RDAdjustmentName <- adjustmentName
    if (adjustmentName != "None") {
      shinyjs::show("rdSelectParam")
    } else {
      shinyjs::hide("rdSelectParam")
    }
    if (!appStatus$StateUploading) {
      invalidateAdjustments()
    }
  })

  # EVENT: Button "Edit parameters" clicked
  observeEvent(input[["miSelectParam"]], {
    vals$editedAdjustmentName <- appStatus$MIAdjustmentName
  })

  # EVENT: Button "Edit parameters" clicked
  observeEvent(input[["rdSelectParam"]], {
    vals$editedAdjustmentName <- appStatus$RDAdjustmentName
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
      editedAdjustmentSpec <- appStatus$AdjustmentSpecs[[vals$editedAdjustmentName]]
      vals$editedAdjustmentParamsWidgets <- GetAdjustParamsWidgets(editedAdjustmentSpec)

      showModal(modalDialog(
        title = "Edit adjustment parameters",
        uiOutput(ns("adjustmentParams")),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("paramsDlgOk"), "OK", style = "background-color: #69b023; color: white"),
          actionButton(ns("paramsDlgCancel"), "Cancel")
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
    adjustmentParams <- appStatus$AdjustmentSpecs[[vals$editedAdjustmentName]]$Parameters
    for (paramName in names(vals$editedAdjustmentParamsWidgets)) {
      adjustmentParams[[paramName]]$value <- input[[paramName]]
    }
    # Save parameters in the selected adjustment object
    appStatus$AdjustmentSpecs[[vals$editedAdjustmentName]]$Parameters <- adjustmentParams

    # Clean up
    vals$editedAdjustmentName <- "None"
    vals$editedAdjustmentParamsWidgets <- list()
    invalidateAdjustments()
    removeModal()
  })

  # Adjustment parameters editing dialog CLOSE through Cancel event
  observeEvent(input[["paramsDlgCancel"]], {
    # Clean up
    vals$editedAdjustmentName <- "None"
    vals$editedAdjustmentParamsWidgets <- list()
    removeModal()
  })

  observe({
    if ((appStatus$MIAdjustmentName != "None" || appStatus$RDAdjustmentName != "None") &&
        appStatus$AttrMappingValid) {
      shinyjs::enable("runAdjustBtn")
    } else {
      shinyjs::disable("runAdjustBtn")
    }
  })

  # EVENT: Button "Run adjustments" clicked
  observeEvent(input[["runAdjustBtn"]], {
    inputData <- req(appStatus$InputData)
    if (appStatus$YearRangeApply) {
      yearRange <- appStatus$YearRange
    } else {
      yearRange <-  NULL
    }

    adjustmentSpecs <- list()
    if (appStatus$MIAdjustmentName != "None") {
      adjustmentSpecs[[appStatus$MIAdjustmentName]] <- appStatus$AdjustmentSpecs[[appStatus$MIAdjustmentName]]
    }
    if (appStatus$RDAdjustmentName != "None") {
      adjustmentSpecs[[appStatus$RDAdjustmentName]] <- appStatus$AdjustmentSpecs[[appStatus$RDAdjustmentName]]
    }

    shinyjs::disable("runAdjustBtn")
    shinyjs::enable("cancelAdjustBtn")

    appStatus$AdjustedData <- NULL

    appStatus$RunLog <- ""
    appStatus$IntermReport <- ""

    # Show progress message during task start
    prog <- Progress$new(session)
    prog$set(message = "Running adjustments...", value = 0.1)

    startTime <- Sys.time()

    if (isLinux) {
      task <<- CreateTask({
        RunAdjustments(
          inputData$Table,
          adjustmentSpecs = adjustmentSpecs,
          yearRange = yearRange)
      })
    } else {
      task <<- CreateTask(function(x, y, yearRange) {
        hivEstimatesAccuracy::RunAdjustments(
          data = x,
          adjustmentSpecs = y,
          yearRange = yearRange)
      },
      args = list(inputData$Table,
                  adjustmentSpecs,
                  yearRange))
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
        appStatus$IntermReport <- HTML(intermReport)
        appStatus$RunLog <- "Done"
      } else {
        appStatus$AdjustedData <- NULL
        appStatus$RunLog <- "Adjustments cancelled"
      }
      appStatus$RunLog <- paste(paste("Start time  :", FormatTime(startTime)),
                                paste("End time    :", FormatTime(endTime)),
                                paste("Elapsed time:", FormatDiffTime(endTime - startTime)),
                                paste(""),
                                appStatus$RunLog,
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

  output[["rerunInfo"]] <- renderUI({
    inputDataAvailable <- appStatus$AttrMappingValid
    adjustedDataAvailable <- !is.null(appStatus$AdjustedData)

    if (!inputDataAvailable) {
      return(p("Please, apply attribute mapping before proceeding with adjustments."))
    } else if (!adjustedDataAvailable) {
      return(p("Input data or adjustment parameters changed. Please, re-run adjustments."))
    } else {
      return(NULL)
    }
  })

  # Output intermediate report when it has changed
  output[["intermReport"]] <- renderUI({
    if (appStatus$IntermReport != "") {
      intermReportHTML <- box(
        class = "intermReport",
        width = 12,
        title = "Intermediate outputs of adjustments",
        solidHeader = FALSE,
        collapsible = TRUE,
        status = "warning",
        appStatus$IntermReport
      )
    } else {
      intermReportHTML <- NULL
    }
    return(intermReportHTML)
  })

  # Populate adjustment parameter widgets in the editing dialog
  output[["runLog"]] <- renderUI({
    if (appStatus$RunLog != "") {
      runLogHTML <- box(
        width = 12,
        title = "Run log",
        solidHeader = FALSE,
        collapsible = TRUE,
        status = "warning",
        tags$pre(appStatus$RunLog)
      )
    } else {
      runLogHTML <- NULL
    }
    return(runLogHTML)
  })

  return(NULL)
}
