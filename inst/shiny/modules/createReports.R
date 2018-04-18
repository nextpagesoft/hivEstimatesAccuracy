# Module globals
reportFileNames <- GetReportFileNames()
reportNames <- names(reportFileNames)
skipWidgetParamNames <- c("AdjustedData", "nsdf")

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
      uiOutput(ns("reportSelection"))),
    uiOutput(ns("report"))
  )
}

# Server logic
createReports <- function(input, output, session, adjustedData)
{
  # Store reactive values
  vals <- reactiveValues(selectedReportName = NULL,
                         reportParamsWidgets = list(),
                         reportParams = list())

  # Output reports selection
  output[["reportSelection"]] <- renderUI({
    isolate({
      ns <- session$ns

      # Get adjustment list
      reportSelection <- fluidRow(
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
          actionButton(ns("createReportBtn"), "Create")
        )
      )

      return(reportSelection)
    })
  })

  # Get selected report name on change
  observeEvent(input[["select"]], {
    ns <- session$ns
    if (input$select != "") {
      vals$selectedReportName <- input$select
      if (vals$selectedReportName != "") {
        reportFileName <- reportFileNames[vals$selectedReportName]
        if (file.exists(reportFileName)) {
          reportMdLines <- readLines(reportFileName, warn = FALSE)
          paramSpecs <- knitr::knit_params(reportMdLines)

          vals$reportParamsWidgets <-
            GetParamWidgets(paramSpecs,
                            params = vals$reportParams[[vals$selectedReportName]],
                            skipParamNames = skipWidgetParamNames,
                            ns = ns)
        }
      }
    }
  })

  # Output adjustment parameters when selected adjustments changed
  output[["reportParams"]] <- renderUI({
    vals$reportParamsWidgets
  })

  # EVENT: Button "Open parameters dialog" clicked
  observeEvent(input[["openParamsDlg"]], {
    ns <- session$ns

    showModal(modalDialog(
      title = "Edit report parameters",
      uiOutput(ns("reportParams")),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("paramsDlgOk"), "OK")
      )
    ))
  })

  observeEvent(input[["paramsDlgOk"]], {
    reportParams <- list()
    for (paramName in names(vals$reportParamsWidgets)) {
      reportParams[[paramName]] <- input[[paramName]]
    }
    vals$reportParams[[vals$selectedReportName]] <- reportParams

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

    removeModal()
  })

  # EVENT: Button "Create report" clicked
  report <- eventReactive(input[["createReportBtn"]], {

    adjustedData <- adjustedData()
    if (!is.null(adjustedData)) {
      withProgress(message = "Creating report",
                   detail = "The report will be displayed shortly.",
                   value = 0, {
                     # Define parameters
                     params <- append(list(AdjustedData = adjustedData()),
                                      vals$reportParams[[vals$selectedReportName]])

                     setProgress(0.1)

                     report <- RenderReportToHTML(reportFileNames[vals$selectedReportName],
                                                  params = params)

                     setProgress(1)
                   })

      # Create report
      return(report)
    }
  })

  # Output report when it has changed
  output[["report"]] <- renderUI({
    # Respond to report change
    report <- report()
    if (!is.null(report)) {
      isolate({
        ns <- session$ns

        # Get adjusted data details
        reportDiv <-
          div(id = "reportDiv",
              downloadButton(ns("downloadHtmlReport"), "Download as HTML"),
              downloadButton(ns("downloadPdfReport"), "Download as PDF"),
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

          params <- append(list(AdjustedData = adjustedData()),
                           vals$reportParams[[vals$selectedReportName]])
          setProgress(0.1)

          # Knit the document, passing in the "data" list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          RenderReportToFile(reportFileNames[vals$selectedReportName],
                             format = format,
                             params = params,
                             output_file = file)
          setProgress(1)
        })
      }
    )
  }

  # Respond to report download button clicks
  output[["downloadHtmlReport"]] <- reportDownloadHandler("html_document")
  output[["downloadPdfReport"]] <- reportDownloadHandler("pdf_document")
  output[["downloadWordReport"]] <- reportDownloadHandler("word_document")

  return(report)
}
