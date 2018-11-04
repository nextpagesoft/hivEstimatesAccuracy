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
          value = c(1980, 2025),
          step = 1,
          sep = "",
          width = "612px",
          round = TRUE
        ),
        div(
          checkboxInput(ns("filterChkBox"),
                        "Apply filter in adjustments",
                        FALSE),
          textOutput(ns("filterInfo")),
          style = "position: absolute; top: 62px; left: 700px"
        ),
        style = "margin-left: 22px; position: relative"
      ),
      uiOutput(ns("diagYearDensityOutput")),
      # div(textOutput(ns("filterInfo"))
      #     style = "margin-left: 22px"),
      tagList(
        uiOutput(ns("missPlotDiv")),
        uiOutput(ns("missPlotRDOutput")),
        uiOutput(ns("delayDensityOutput")),
        uiOutput(ns("meanDelayOutput"))
      ),
      type = 7,
      proxy.height = "60px"
    )
  )
}

# Server logic
dataSummary <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  invalidateAdjustments <- function() {
    appStatus$AdjustedData <- NULL
  }

  observeEvent(appStatus$InputData, {
    appStatus$YearRange <-
      appStatus$InputData$Table[,
        c(max(1980, min(DateOfDiagnosisYear, na.rm = TRUE)),
          min(2025, max(DateOfDiagnosisYear, na.rm = TRUE)))]
    if (appStatus$YearRangeApply) {
      invalidateAdjustments()
    }
  })

  # Store filter settings
  observeEvent(input[['filterChkBox']], {
    freezeReactiveValue(appStatus, 'YearRangeApply')
    appStatus$YearRangeApply <- input[['filterChkBox']]
    invalidateAdjustments()
  })

  observeEvent(input[['yearRange']], {
    freezeReactiveValue(appStatus, 'YearRange')
    appStatus$YearRange <- input[['yearRange']]
    if (appStatus$YearRangeApply) {
      invalidateAdjustments()
    }
  })

  # Restore filter settings
  observeEvent(appStatus$YearRange, {
    freezeReactiveValue(input, 'yearRange')
    updateSliderInput(session, 'yearRange', value = appStatus$YearRange)
    if (appStatus$YearRangeApply) {
      invalidateAdjustments()
    }
  })

  observeEvent(appStatus$YearRangeApply, {
    freezeReactiveValue(input, 'filterChkBox')
    updateCheckboxInput(session, 'filterChkBox', value = appStatus$YearRangeApply)
    invalidateAdjustments()
  })

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
    if (appStatus[["AttrMappingValid"]]) {
      missPlotsTotal <- artifacts()$MissPlotsTotal
      missPlotsByGender <- artifacts()$MissPlotsByGender
      plotCounter <- 0
      widgets[[length(widgets) + 1]] <- h1("1. Missing data summary: key variables")
      widgets[[length(widgets) + 1]] <- p("Percentages of cases for which the information was not available (missing) for one or more of the key variables: CD4 count, transmission category, migrant status or age.")

      plotCounter <- plotCounter + 1
      widgets[[length(widgets) + 1]] <- h2(sprintf("1.%d. All cases within selected time period", plotCounter))
      if (!is.null(missPlotsTotal)) {
        widgets[[length(widgets) + 1]] <- plotOutput(ns("missPlotsTotal"))
      } else {
        widgets[[length(widgets) + 1]] <- p("This plot cannot be created due to insufficient data.")
      }
      plotCounter <- plotCounter + 1
      widgets[[length(widgets) + 1]] <- h2(sprintf("1.%d. By gender", plotCounter))
      if (!is.null(missPlotsByGender)) {
        widgets[[length(widgets) + 1]] <- h3(sprintf("1.%d.1 Female cases within selected time period", plotCounter))
        widgets[[length(widgets) + 1]] <- plotOutput(ns("missPlotsGenderF"))
        widgets[[length(widgets) + 1]] <- h3(sprintf("1.%d.2 Male cases within selected time period", plotCounter))
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
                       widths = c(3, 1, 4))
  })

  output[["missPlotsGenderF"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsByGender$F,
                       cols = 3,
                       widths = c(3, 1, 4))
  })

  output[["missPlotsGenderM"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsByGender$M,
                       cols = 3,
                       widths = c(3, 1, 4))
  })

  output[["missPlotRDOutput"]] <- renderUI({
    req(appStatus[["AttrMappingValid"]])
    req(artifacts()$MissPlotsRD)

    tagList(
      h1("2. Missing data summary: reporting delay variables"),
      p("Percentages of cases for which the information was not available (missing) for one or more of the variables used for reporting delay calculations: Diagnosis year, Diagnosis quarter, Notification year, Notification quarter."),
      plotOutput(ns("MissPlotsRD"))
    )
  })

  output[["MissPlotsRD"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsRD,
                       cols = 3,
                       widths = c(3, 1, 4))
  })

  output[["delayDensityOutput"]] <- renderUI({
    if (appStatus$AttrMappingValid) {
      delayDensFullPlot <- artifacts()$DelayDensFullPlot
      delayDensShortPlot <- artifacts()$DelayDensFullPlot
      if (!is.null(delayDensShortPlot) & !is.null(delayDensShortPlot)) {
        elem <- plotOutput(ns("delayDensityPlot"))
      } else {
        elem <- p("This plot cannot be created due to insufficient data.")
      }
      tagList(
        h1("3. Trends in reporting delay by notification time"),
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
    if (appStatus$AttrMappingValid) {
      meanDelayPlot <- artifacts()$MeanDelayPlot
      if (!is.null(meanDelayPlot)) {
        elem <- plotOutput(ns("meanDelayPlot"))
      } else {
        elem <- p("This plot cannot be created due to insufficient data.")
      }
      tagList(
        h1("4. Observed delay by notification time"),
        elem
      )

    } else {
      return(NULL)
    }
  })

  output[["meanDelayPlot"]] <- renderPlot({
    artifacts()$MeanDelayPlot
  })
}
