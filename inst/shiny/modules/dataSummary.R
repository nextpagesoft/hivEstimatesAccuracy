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
        proxy.height = "60px")
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
    GetDataSummaryArtifacts(inputData()$Table)
  })

  output[["missPlotDiv"]] <- renderUI({
    if (appStatus$AttributeMappingValid) {
      missPlots <- artifacts()$MissPlots
      if (!is.null(missPlots)) {
        tagList(
          h3("1. Missing data summary"),
          plotOutput(ns("missPlots"))
        )
      }
    } else {
      p("Please, upload input data and apply attribute mapping in \"Input data upload\" tab first.")
    }
  })

  output[["missPlots"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlots,
                       cols = 3,
                       widths = c(3, 3, 2))
  })

  output[["delayDensityOutput"]] <- renderUI({
    if (appStatus$AttributeMappingValid) {
      delayDensFullPlot <- artifacts()$DelayDensFullPlot
      delayDensShortPlot <- artifacts()$DelayDensFullPlot
      if (!is.null(delayDensShortPlot) & !is.null(delayDensShortPlot)) {
        elem <- plotOutput(ns("delayDensityPlot"))
      } else {
        elem <- p("This plot cannot be created due to missingness in the provided diagnosis and notification times.")
      }
      tagList(
        h3("2. Observed delay density"),
        elem
      )
    } else {
      return(NULL)
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
    if (appStatus$AttributeMappingValid) {
      meanDelayPlot <- artifacts()$MeanDelayPlot
      if (!is.null(meanDelayPlot)) {
        elem <- plotOutput(ns("meanDelayPlot"))
      } else {
        elem <- p("This plot cannot be created due to missingness in the provided diagnosis and notification times.")
      }
      tagList(
        h3("3. Observed delay by notification time"),
        elem
      )

    } else {
      return(NULL)
    }
  })

  output[["meanDelayPlot"]] <- renderPlot({
    artifacts()$MeanDelayPlot
  })

  output[["inputDataTableBox"]] <- renderUI({
    if (appStatus$AttributeMappingValid) {
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
