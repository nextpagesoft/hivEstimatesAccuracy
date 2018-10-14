# Module globals
currYear <- year(Sys.time())

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
      div(
        sliderInput(
          ns("yearRange"),
          label = h3("Filter data on year of diagnosis"),
          min = 1980,
          max = 2025,
          value = c(2000, currYear),
          step = 1,
          sep = "",
          width = "612px",
          round = TRUE
        ),
        style = "margin-left: 22px"
      ),
      uiOutput(ns("diagYearDensityOutput")),
      tags$small(textOutput(ns("filterInfo"))),
      tagList(
        uiOutput(ns("missPlotDiv")),
        uiOutput(ns("missPlotRDOutput")),
        uiOutput(ns("delayDensityOutput")),
        uiOutput(ns("meanDelayOutput"))
      ),
      type = 7,
      proxy.height = "60px"
    ),
    uiOutput(ns("inputDataTableBox"))
  )
}

# Server logic
dataSummary <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  dataSelection <- reactive({
    yearRange <- req(input[['yearRange']])
    req(appStatus$InputData$Table)[, DateOfDiagnosisYear %between% yearRange]
  })

  artifacts <- reactive({
    GetDataSummaryArtifacts(appStatus$InputData$Table[dataSelection()])
  })

  output[["filterInfo"]] <- renderText({
    dataSelection <- dataSelection()
    sprintf("Selected observations: %d out of %d", sum(dataSelection), length(dataSelection))
  })

  output[["diagYearDensityOutput"]] <- renderUI({
    req(appStatus$InputData$Table)
    req(input[['yearRange']])
    plotOutput(ns("diagYearDensityPlot"),
               width = "700px",
               height = "150px")
  })

  output[["diagYearDensityPlot"]] <- renderPlot({
    GetDiagnosisYearDensityPlot(plotData = appStatus$InputData$Table,
                                markerLocations = input$yearRange)
  })

  output[["missPlotDiv"]] <- renderUI({
    widgets <- tagList()
    if (appStatus[["AttributeMappingValid"]]) {
      missPlotsTotal <- artifacts()$MissPlotsTotal
      missPlotsByGender <- artifacts()$MissPlotsByGender
      plotCounter <- 0
      widgets[[length(widgets) + 1]] <- h3("1. Missing data summary: key variables")
      widgets[[length(widgets) + 1]] <- p("Percentages of cases for which the information was not available (missing) for one or more of the key variables: CD4 count, transmission category, migrant status or age.")

      plotCounter <- plotCounter + 1
      widgets[[length(widgets) + 1]] <- h4(sprintf("1.%d. All cases within selected time period", plotCounter))
      if (!is.null(missPlotsTotal)) {
        widgets[[length(widgets) + 1]] <- plotOutput(ns("missPlotsTotal"))
      } else {
        widgets[[length(widgets) + 1]] <- p("This plot cannot be created due to insufficient data.")
      }
      plotCounter <- plotCounter + 1
      widgets[[length(widgets) + 1]] <- h4(sprintf("1.%d. By gender", plotCounter))
      if (!is.null(missPlotsByGender)) {
        widgets[[length(widgets) + 1]] <- h4(sprintf("1.%d.1 Female cases within selected time period", plotCounter))
        widgets[[length(widgets) + 1]] <- plotOutput(ns("missPlotsGenderF"))
        widgets[[length(widgets) + 1]] <- h4(sprintf("1.%d.2 Male cases within selected time period", plotCounter))
        widgets[[length(widgets) + 1]] <- plotOutput(ns("missPlotsGenderM"))
      } else {
        widgets[[length(widgets) + 1]] <- p("This plot cannot be created due to insufficient data.")
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

  output[["missPlotRDOutput"]] <- renderUI({
    req(appStatus[["AttributeMappingValid"]])
    req(artifacts()$MissPlotsRD)

    tagList(
      h3("2. Missing data summary: reporting delay variables"),
      p("Percentages of cases for which the information was not available (missing) for one or more of the variables used for reporting delay calculations: Diagnosis year, Diagnosis quarter, Notification year, Notification quarter."),
      plotOutput(ns("MissPlotsRD"))
    )
  })

  output[["MissPlotsRD"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsRD,
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
        elem <- p("This plot cannot be created due to insufficient data.")
      }
      tagList(
        h3("3. Trends in reporting delay by notification time"),
        p("Average reporting delay for cases notified within a quarter and the upper bound for typical average delay values. Quarters when the average delay exceeds the upper bound may indicate cleaning events in surveillance."),
        elem
      )
    } else {
      return(NULL)
    }
  })

  output[["delayDensityPlot"]] <- renderPlot({
    artifacts()$DelayDensFullPlot
  })

  output[["meanDelayOutput"]] <- renderUI({
    if (appStatus$AttributeMappingValid) {
      meanDelayPlot <- artifacts()$MeanDelayPlot
      if (!is.null(meanDelayPlot)) {
        elem <- plotOutput(ns("meanDelayPlot"))
      } else {
        elem <- p("This plot cannot be created due to insufficient data.")
      }
      tagList(
        h3("4. Observed delay by notification time"),
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

  output[["inputDataTable"]] <- renderDataTable(appStatus$InputData$Table,
                                                options = list(
                                                  dom = '<"top">lirt<"bottom">p',
                                                  autoWidth = FALSE,
                                                  pageLength = 15,
                                                  scrollX = TRUE,
                                                  deferRender = TRUE,
                                                  serverSide = TRUE,
                                                  scroller = FALSE))
}
