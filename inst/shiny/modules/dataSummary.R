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
      sliderInput(ns("yearRange"),
                  label = h3("Filter data on year of diagnosis"),
                  min = 1980,
                  max = 2025,
                  value = c(1980, 2025),
                  step = 1,
                  sep = "",
                  round = TRUE),
      textOutput(ns("filterInfo")),
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

  dataSelection <- reactive({
    yearRange <- input$yearRange
    inputData()$Table[, between(DateOfDiagnosisYear, yearRange[1], yearRange[2])]
  })

  artifacts <- reactive({
    GetDataSummaryArtifacts(inputData()$Table[dataSelection()])
  })

  output[["filterInfo"]] <- renderText({
    dataSelection <- dataSelection()
    sprintf("Selected %d out of %d", sum(dataSelection), length(dataSelection))
  })

  output[["missPlotDiv"]] <- renderUI({
    widgets <- tagList()
    if (appStatus$AttributeMappingValid) {
      missPlotsTotal <- artifacts()$MissPlotsTotal
      missPlotsByGender <- artifacts()$MissPlotsByGender
      plotCounter <- 0
      if (!is.null(missPlotsTotal) | !is.null(missPlotsByGender)) {
        widgets[[length(widgets) + 1]] <- h3("1. Missing data summary")

        if (!is.null(missPlotsTotal)) {
          plotCounter <- plotCounter + 1
          widgets[[length(widgets) + 1]] <- h4(sprintf("1.%d. Total plot", plotCounter))
          widgets[[length(widgets) + 1]] <- plotOutput(ns("missPlotsTotal"))
        }
        if (!is.null(missPlotsByGender)) {
          plotCounter <- plotCounter + 1
          widgets[[length(widgets) + 1]] <- h4(sprintf("1.%d. Gender \"Female\"", plotCounter))
          widgets[[length(widgets) + 1]] <- plotOutput(ns("missPlotsGenderF"))
          plotCounter <- plotCounter + 1
          widgets[[length(widgets) + 1]] <- h4(sprintf("1.%d. Gender \"Male\"", plotCounter))
          widgets[[length(widgets) + 1]] <- plotOutput(ns("missPlotsGenderM"))
        }
      }
    } else {
      widgets[[length(widgets) + 1]] <- p("Please, upload input data and apply attribute mapping in \"Input data upload\" tab first.")
    }
    return(widgets)
  })

  output[["missPlotsTotal"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsTotal,
                       cols = 3,
                       widths = c(3, 3, 2))
  })

  output[["missPlotsGenderF"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsByGender$F,
                       cols = 3,
                       widths = c(3, 3, 2))
  })

  output[["missPlotsGenderM"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsByGender$M,
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

  output[["inputDataTable"]] <- renderDataTable(inputData()$Table,
                                                options = list(
                                                  dom = '<"top">lirt<"bottom">p',
                                                  autoWidth = FALSE,
                                                  pageLength = 25,
                                                  scrollX = TRUE,
                                                  deferRender = TRUE,
                                                  serverSide = TRUE,
                                                  scroller = FALSE))
}
