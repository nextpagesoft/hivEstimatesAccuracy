# Module globals
# NONE

# User interface
dataSummaryUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "Input data summary",
      solidHeader = FALSE,
      status = "primary",
      collapsible = TRUE,
      withSpinner(
        tagList(
          uiOutput(ns("missPlotDiv")),
          uiOutput(ns("delayDensityOutput")),
          uiOutput(ns("meanDelayOutput"))
        ),
        type = 7,
        proxy.height = "50px")
    ),
    uiOutput(ns("inputDataTableBox"))
  )
}

# Server logic
dataSummary <- function(input, output, session, appStatus, inputData)
{
  # Get namespace
  ns <- session$ns

  artifacts <- reactive({
    GetDataSummaryArtifacts(inputData())
  })

  output[["missPlotDiv"]] <- renderUI({
    if (appStatus$InputDataUploaded) {
      missPlot <- artifacts()$MissPlot
      if (!is.null(missPlot)) {
        tagList(
          h3("1. Missing data summary"),
          plotOutput(ns("missPlot"))
        )
      }
    } else {
      p("Please, upload input data and apply attribute mapping in \"Input data upload\" tab first.")
    }
  })

  output[["missPlot"]] <- renderPlot({
    artifacts()$MissPlot
  })

  output[["delayDensityOutput"]] <- renderUI({
    if (appStatus$InputDataUploaded) {
      delayDensFullPlot <- artifacts()$DelayDensFullPlot
      delayDensShortPlot <- artifacts()$DelayDensFullPlot
      if (!is.null(delayDensShortPlot) & !is.null(delayDensShortPlot)) {
        tagList(
          h3("2. Observed delay density"),
          plotOutput(ns("delayDensityPlot"))
        )
      } else {
        p("This plot cannot be created due to missingness in the provided diagnosis and notification times.")
      }
    }
  })

  output[["delayDensityPlot"]] <- renderPlot({
    delayDensFullPlot <- artifacts()$DelayDensFullPlot
    delayDensShortPlot <- artifacts()$DelayDensShortPlot
    PlotMultipleCharts(list(delayDensFullPlot,
                            delayDensShortPlot),
                       cols = 2)
  })

  output[["meanDelayOutput"]] <- renderUI({
    if (appStatus$InputDataUploaded) {
      meanDelayPlot <- artifacts()$MeanDelayPlot
      if (!is.null(meanDelayPlot)) {
        tagList(
          h3("3. Observed delay by notification time"),
          plotOutput(ns("meanDelayPlot"))
        )
      } else {
        p("This plot cannot be created due to missingness in the provided diagnosis and notification times.")
      }
    }
  })

  output[["meanDelayPlot"]] <- renderPlot({
    artifacts()$MeanDelayPlot
  })

  output[["inputDataTableBox"]] <- renderUI({
    if (appStatus$InputDataUploaded) {
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

  output[["inputDataTable"]] <- renderDataTable(inputData(),
                                                     options = list(
                                                       autoWidth = FALSE,
                                                       pageLength = 10,
                                                       scrollX = TRUE,
                                                       deferRender = TRUE,
                                                       serverSide = TRUE,
                                                       scroller = FALSE))
}
