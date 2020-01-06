# Module globals
reportFileNames <- GetReportFileNames()
reportNames <- names(reportFileNames)
skipWidgetParamNames <- c("AdjustedData", "Artifacts", "nsdf")

# User interface
createReportsUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "Create report",
      solidHeader = FALSE,
      collapsible = TRUE,
      status = "primary",
      fluidRow(
        column(6,
               selectInput(ns("select"),
                           label = NULL,
                           choices = reportNames,
                           selected = 1,
                           selectize = TRUE)),
        column(6,
               actionLink(ns("openParamsDlg"), "Edit parameters"),
               style = "padding-top: 7px"),
        column(12,
               uiOutput(ns("rerunInfo")),
               actionButton(ns("createReportBtn"), "Create",
                            style = "background-color: #69b023; color: white"),
               shinyjs::disabled(actionButton(ns("cancelReportBtn"), "Cancel"))
        )
      )),
    uiOutput(ns("report"))
  )
}

# Server logic
createReports <- function(input, output, session, appStatus)
{
  ns <- session$ns

  # Make "task" behave like a reactive value
  makeReactiveBinding("reportTask")
  reportTask <- NULL

  # Store reactive values
  vals <- reactiveValues(selectedReportName = NULL,
                         reportParamsWidgets = list(),
                         reportParams = list(),
                         reportParamsFull = list())

  invalidateReport <- function() {
    appStatus$Report <- NULL
  }

  adjustedDataAvailable <- reactive({
    !is.null(appStatus$AdjustedData)
  })

  observe({
    if (adjustedDataAvailable()) {
      shinyjs::enable("createReportBtn")
    } else {
      shinyjs::disable("createReportBtn")
    }
  })

  # Get selected report name on change
  observeEvent(input[["select"]], {
    vals$selectedReportName <- req(input$select)

    # Populate widgets with default values for a next dialog open
    reportFileName <- reportFileNames[vals$selectedReportName]
    if (file.exists(reportFileName)) {
      reportMdLines <- readLines(reportFileName, warn = FALSE)
      paramSpecs <- knitr::knit_params(reportMdLines)

      vals$reportParams[[vals$selectedReportName]] <-
        GetReportDefaultParams(params = paramSpecs,
                               skipParamNames = skipWidgetParamNames)

      vals$reportParamsWidgets <-
        GetParamWidgets(paramSpecs,
                        params = vals$reportParams[[vals$selectedReportName]],
                        skipParamNames = skipWidgetParamNames,
                        ns = ns)

      vals$reportParamsFull <- list()
    }
  })

  # Output adjustment parameters when selected adjustments changed
  output[["reportParams"]] <- renderUI({
    vals$reportParamsWidgets
  })

  # EVENT: Button "Open parameters dialog" clicked
  observeEvent(input[["openParamsDlg"]], {
    showModal(modalDialog(
      title = "Edit report parameters",
      uiOutput(ns("reportParams")),
      easyClose = FALSE,
      footer = tagList(
        actionButton(ns("paramsDlgOk"), "OK", style = "background-color: #69b023; color: white"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input[["paramsDlgOk"]], {
    reportParams <- list()
    for (paramName in names(vals$reportParamsWidgets)) {
      reportParams[[paramName]] <- input[[paramName]]
    }
    vals$reportParams[[vals$selectedReportName]] <- reportParams

    # Populate widgets with updated values for a next dialog open
    reportFileName <- reportFileNames[vals$selectedReportName]
    if (file.exists(reportFileName)) {
      reportMdLines <- readLines(reportFileName, warn = FALSE)
      paramSpecs <- knitr::knit_params(reportMdLines)

      vals$reportParamsWidgets <-
        GetParamWidgets(paramSpecs,
                        params = vals$reportParams[[vals$selectedReportName]],
                        skipParamNames = skipWidgetParamNames,
                        ns = session$ns)
    }

    invalidateReport()
    removeModal()
  })

  output[["rerunInfo"]] <- renderUI({
    adjustedDataAvailable <- adjustedDataAvailable()

    if (!adjustedDataAvailable) {
      return(p("Adjusted data is not available for report. Please, re-run adjustments first."))
    } else if (IsEmptyString(appStatus$Report)) {
      return(p("Adjusted data or report parameters have changed. Please, re-create the report."))
    } else {
      return(NULL)
    }
  })

  # EVENT: Button "Create report" clicked
  observeEvent(input[["createReportBtn"]], {
    adjustedData <- req(appStatus$AdjustedData)
    fileName <- appStatus$FileName
    diagYearRangeApply <- appStatus$DiagYearRangeApply
    diagYearRange <- appStatus$DiagYearRange

    shinyjs::disable("createReportBtn")
    shinyjs::enable("cancelReportBtn")

    appStatus$Report <- ""

    # Show progress message during task start
    prog <- Progress$new(session)
    prog$set(message = "Creating report...", value = 0.5)

    # Define parameters
    params <- append(list(AdjustedData = adjustedData),
                     vals$reportParams[[vals$selectedReportName]])

    if (isLinux) {
      reportTask <<- CreateTask({
        params <- GetMainReportArtifacts(params)
        params <- modifyList(params,
                             list(Artifacts =
                                    list(FileName = fileName,
                                         DiagYearRange = diagYearRange,
                                         DiagYearRangeApply = diagYearRangeApply)))

        report <- RenderReportToHTML(reportFileNames[vals$selectedReportName],
                                     params = params)

        list(Report = report,
             Params = params)
      })
    } else {
      reportTask <<- CreateTask(function(params, fileName, diagYearRange, diagYearRangeApply, reportFileName) {
        params <- hivEstimatesAccuracy::GetMainReportArtifacts(params)
        params <- modifyList(params,
                             list(Artifacts =
                                    list(FileName = fileName,
                                         DiagYearRange = diagYearRange,
                                         DiagYearRangeApply = diagYearRangeApply)))

        report <- hivEstimatesAccuracy::RenderReportToHTML(reportFileName, params = params)

        list(Report = report,
             Params = params)
      },
      args = list(params,
                  fileName,
                  diagYearRange,
                  diagYearRangeApply,
                  reportFileNames[vals$selectedReportName]))
    }

    o <- observe({
      # Only proceed when the task is completed (this could mean success,
      # failure, or cancellation)
      req(reportTask$completed())
      endTime <- Sys.time()

      taskResults <- reportTask$result()

      if (is.list(taskResults)) {
        appStatus$Report <- taskResults$Report
        # Store parameters for reuse when downloading
        vals$reportParamsFull <- taskResults$Params
      }

      reportTask <<- NULL

      # This observer only runs once
      o$destroy()

      # Close the progress indicator and update button state
      prog$close()
      shinyjs::enable("createReportBtn")
      shinyjs::disable("cancelReportBtn")
    })

  })

  # EVENT: Button "Run adjustments" clicked
  observeEvent(input[["cancelReportBtn"]], {
    req(reportTask)$cancel()
  })

  # Output report when it has changed
  output[["report"]] <- renderUI({
    # Respond to report change
    report <- req(appStatus$Report)
    if (!is.null(report)) {
      isolate({
        ns <- session$ns

        # Get adjusted data details
        reportDiv <-
          div(id = "reportDiv",
              downloadButton(ns("downloadHtmlReport"), "Download as HTML"),
              downloadButton(ns("downloadPdfReport"), "Download as PDF"),
              downloadButton(ns("downloadLatexReport"), "Download as Latex"),
              downloadButton(ns("downloadWordReport"), "Download as Word"),
              report
          )

        reportHTML <- box(
          width = 12,
          title = "Report",
          solidHeader = FALSE,
          collapsible = TRUE,
          status = "warning",
          reportDiv)


        return(reportHTML)
      })
    }
  })

  # Define report download handler
  reportDownloadHandler <- function(format) {
    fileExtension <- switch(format,
                            "html_document" = "html",
                            "pdf_document" = "pdf",
                            "latex_document" = "zip",
                            "word_document" = "docx",
                            "txt")

    downloadHandler(
      filename = function() {
        fileName <- sprintf("%s_%s.%s",
                            vals$selectedReportName,
                            GetTimeStamp(),
                            fileExtension)
        return(fileName)
      },

      content = function(file) {
        withProgress(message = "Creating report",
                     detail = "The report file will be available for download shortly.",
                     value = 0, {

          params <- vals$reportParamsFull

          setProgress(0.2)

          # Knit the document, passing in the "data" list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          RenderReportToFile(reportFilePath = reportFileNames[vals$selectedReportName],
                             format = format,
                             params = params,
                             outputFilePath = file)
          setProgress(1)
        })
      }
    )
  }

  # Respond to report download button clicks
  output[["downloadHtmlReport"]] <- reportDownloadHandler("html_document")
  output[["downloadPdfReport"]] <- reportDownloadHandler("pdf_document")
  output[["downloadLatexReport"]] <- reportDownloadHandler("latex_document")
  output[["downloadWordReport"]] <- reportDownloadHandler("word_document")

  return(NULL)
}
