# Module globals
genderLabels <- c("M" = "Male", "F" = "Female", "O" = "Other")

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
          ns("diagYearRange"),
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
          checkboxInput(ns("diagYearFilterChkBox"),
                        "Apply filter in adjustments",
                        FALSE),
          style = "position: absolute; top: 62px; left: 700px"
        ),
        style = "margin-left: 22px; position: relative"
      ),
      uiOutput(ns("diagYearDensityOutput")),
      div(
        sliderInput(
          ns("notifQuarterRange"),
          label = h3("Filter data on quarter of notification"),
          min = 1980,
          max = 2025,
          value = c(1980, 2025),
          step = 0.25,
          sep = "",
          width = "612px",
          round = -2
        ),
        div(
          checkboxInput(ns("notifQuarterFilterChkBox"),
                        "Apply filter in adjustments",
                        FALSE),
          textOutput(ns("notifQuarterFilterInfo")),
          style = "position: absolute; top: 62px; left: 700px"
        ),
        style = "margin-left: 22px; position: relative"
      ),
      uiOutput(ns("notifQuarterDensityOutput")),
      div(textOutput(ns("filterInfo")),
          style = "margin-left: 22px; font-weight: bold"),
      withSpinner(
        tagList(
          uiOutput(ns("missPlotDiv")),
          uiOutput(ns("missPropOutput")),
          uiOutput(ns("missPlotRDOutput")),
          uiOutput(ns("delayDensityOutput")),
          uiOutput(ns("meanDelayOutput"))),
        type = 7,
        proxy.height = "40px"
      )
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

    diagYearDataRange <- appStatus$InputData$Table[,
      c(min(DateOfDiagnosisYear, na.rm = TRUE),
        max(DateOfDiagnosisYear, na.rm = TRUE))]
    notifQuarterDataRange <- appStatus$InputData$Table[,
      c(floor(min(NotificationTime, na.rm = TRUE)),
        ceiling(max(NotificationTime, na.rm = TRUE)))]

    if (identical(diagYearDataRange, c(NA, NA))) {
      diagYearDataRange <- c(1980, 2025)
    }
    if (identical(notifQuarterDataRange, c(NA, NA))) {
      notifQuarterDataRange <- c(1980, 2025)
    }

    freezeReactiveValue(input, 'diagYearRange')
    updateSliderInput(session, 'diagYearRange',
                      min = diagYearDataRange[1],
                      max = diagYearDataRange[2])

    freezeReactiveValue(input, 'notifQuarterRange')
    updateSliderInput(session, 'notifQuarterRange',
                      min = notifQuarterDataRange[1],
                      max = notifQuarterDataRange[2])

    appStatus$DiagYearRange <- diagYearDataRange
    appStatus$NotifQuarterRange <- notifQuarterDataRange

    if (appStatus$DiagYearRangeApply || appStatus$NotifQuarterRangeApply) {
      invalidateAdjustments()
    }
  })

  # Store filter settings
  observeEvent(input[['diagYearFilterChkBox']], {
    freezeReactiveValue(appStatus, 'DiagYearRangeApply')
    appStatus$DiagYearRangeApply <- input[['diagYearFilterChkBox']]
    invalidateAdjustments()
  })

  observeEvent(input[['diagYearRange']], {
    freezeReactiveValue(appStatus, 'DiagYearRange')
    appStatus$DiagYearRange <- input[['diagYearRange']]

    if (appStatus$DiagYearRangeApply) {
      invalidateAdjustments()
    }
  })

  # Restore filter settings
  observeEvent(appStatus$DiagYearRange, {
    freezeReactiveValue(input, 'diagYearRange')
    updateSliderInput(session, 'diagYearRange', value = appStatus$DiagYearRange)
    if (appStatus$DiagYearRangeApply) {
      invalidateAdjustments()
    }
  })

  observeEvent(appStatus$DiagYearRangeApply, {
    freezeReactiveValue(input, 'diagYearFilterChkBox')
    updateCheckboxInput(session, 'diagYearFilterChkBox', value = appStatus$DiagYearRangeApply)
    invalidateAdjustments()
  })

  # Store filter settings
  observeEvent(input[['notifQuarterFilterChkBox']], {
    freezeReactiveValue(appStatus, 'NotifQuarterRangeApply')
    appStatus$NotifQuarterRangeApply <- input[['notifQuarterFilterChkBox']]
    invalidateAdjustments()
  })

  observeEvent(input[['notifQuarterRange']], {
    freezeReactiveValue(appStatus, 'NotifQuarterRange')
    appStatus$NotifQuarterRange <- input[['notifQuarterRange']]
    if (appStatus$NotifQuarterRangeApply) {
      invalidateAdjustments()
    }
  })

  # Restore filter settings
  observeEvent(appStatus$NotifQuarterRange, {
    freezeReactiveValue(input, 'notifQuarterRange')
    updateSliderInput(session, 'notifQuarterRange', value = appStatus$NotifQuarterRange)
    if (appStatus$NotifQuarterRangeApply) {
      invalidateAdjustments()
    }
  })

  observeEvent(appStatus$NotifQuarterRangeApply, {
    freezeReactiveValue(input, 'notifQuarterFilterChkBox')
    updateCheckboxInput(session, 'notifQuarterFilterChkBox', value = appStatus$NotifQuarterRangeApply)
    invalidateAdjustments()
  })

  output[["notifQuarterFilterInfo"]] <- renderText({
    diagYearRange <- input[['diagYearRange']]
    notifQuarterRange <- input[['notifQuarterRange']]

    if (diagYearRange[1] > notifQuarterRange[1]) {
      sprintf("Start notification quarter should not be before diagnosis year (%d)", diagYearRange[1])
    } else {
      ""
    }
  })

  # Output density plots
  output[["diagYearDensityOutput"]] <- renderUI({
    req(appStatus$InputData$Table)
    req(input[['diagYearRange']])
    plotOutput(ns("diagYearDensityPlot"),
               width = "700px",
               height = "100px")
  })

  output[["diagYearDensityPlot"]] <- renderPlot({
    GetDiagnosisYearDensityPlot(plotData = appStatus$InputData$Table,
                                markerLocations = input$diagYearRange)
  })

  output[["notifQuarterDensityOutput"]] <- renderUI({
    req(appStatus$InputData$Table)
    req(input[['notifQuarterRange']])
    plotOutput(ns("notifQuarterDensityPlot"),
               width = "700px",
               height = "100px")
  })

  output[["notifQuarterDensityPlot"]] <- renderPlot({
    GetNotificationQuarterDensityPlot(plotData = appStatus$InputData$Table,
                                      markerLocations = input$notifQuarterRange)
  })

  dataSelection <- reactive({
    diagYearRange <- req(input[['diagYearRange']])
    notifQuarterRange <- req(input[['notifQuarterRange']])

    req(appStatus$InputData$Table)[, is.na(DateOfDiagnosisYear) |
                                     is.na(NotificationTime) |
                                     (DateOfDiagnosisYear %between% diagYearRange &
                                        NotificationTime %between% notifQuarterRange)]

    # req(appStatus$InputData$Table)[, DateOfDiagnosisYear %between% diagYearRange &
    #                                  NotificationTime %between% notifQuarterRange]
  })

  output[["filterInfo"]] <- renderText({
    dataSelection <- dataSelection()
    sprintf("Number of selected observations: %d out of %d", sum(dataSelection), length(dataSelection))
  })

  artifacts <- reactive({
    dataSelection <- dataSelection()
    print(dataSelection)
    inputData <- appStatus$InputData$Table[dataSelection]
    print(inputData[, unique(DateOfNotificationYear)])
    PreProcessInputDataBeforeAdjustments(inputData)
    GetDataSummaryArtifacts(inputData)
  })

  output[["missPlotDiv"]] <- renderUI({
    artifacts <- artifacts()

    isolate({

      widgets <- tagList()
      if (appStatus[["AttrMappingValid"]]) {
        missPlotsTotal <- artifacts$MissPlotsTotal
        missPlotsByGender <- artifacts$MissPlotsByGender
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
          # i <- 1
          genderCounter <- 0
          for (i in seq_along(genderLabels)) {
            gender <- names(genderLabels)[i]
            if (gender %in% names(artifacts$MissPlotsByGender)) {
              genderCounter <- genderCounter + 1
              genderLabel <- genderLabels[gender]
              plotOutputName <- sprintf("missPlotsGender%s", gender)

              widgets[[length(widgets) + 1]] <- h3(sprintf("1.%d.%d %s cases within selected time period",
                                                           plotCounter, genderCounter, genderLabel))
              widgets[[length(widgets) + 1]] <- plotOutput(ns(plotOutputName))
            }
          }
        } else {
          widgets[[length(widgets) + 1]] <- p("This plot cannot be created due to insufficient data.")
        }
      } else {
        widgets[[length(widgets) + 1]] <- p("Please, upload input data and apply attribute mapping in \"Input data upload\" tab first.")
      }

      widgets
    })
  })

  output[["missPlotsTotal"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsTotal,
                       cols = 3,
                       widths = c(3, 1, 4))
  })

  output[["missPlotsGenderM"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsByGender$M,
                       cols = 3,
                       widths = c(3, 1, 4))
  })

  output[["missPlotsGenderF"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsByGender$F,
                       cols = 3,
                       widths = c(3, 1, 4))
  })

  output[["missPlotsGenderO"]] <- renderPlot({
    PlotMultipleCharts(plots = artifacts()$MissPlotsByGender$O,
                       cols = 3,
                       widths = c(3, 1, 4))
  })

  output[["missPropOutput"]] <- renderUI({
    if (appStatus$AttrMappingValid) {
      missPropPlot <- artifacts()$MissPropPlot
      if (!is.null(missPropPlot)) {
        elem <- plotOutput(ns("missPropPlot"))
      } else {
        elem <- p("This plot cannot be created due to insufficient data.")
      }
      tagList(
        h1("2. Proportion of missing by diagnosis year"),
        elem
      )
    } else {
      return(NULL)
    }
  })

  output[["missPropPlot"]] <- renderPlot({
    artifacts()$MissPropPlot
  })

  output[["missPlotRDOutput"]] <- renderUI({
    req(appStatus[["AttrMappingValid"]])
    req(artifacts()$MissPlotsRD)

    tagList(
      h1("3. Missing data summary: reporting delay variables"),
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
        h1("4. Trends in reporting delay by notification time"),
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
        h1("5. Observed delay by notification time"),
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
