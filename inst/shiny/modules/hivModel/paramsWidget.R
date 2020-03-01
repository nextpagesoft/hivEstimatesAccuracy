# Module globals
# NONE

# User interface
paramsWidgetUI <- function(id)
{
  rangeMinYear <- 1975L
  rangeMaxYear <- 2025L

  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      h1('Incidence Method Advanced Parameters'),
      column(
        width = 6,
        fluidRow(
          h2('Range of calculations'),
          wellPanel(
            sliderInput(
              ns('rangeMain'),
              label = NULL,
              min = rangeMinYear,
              max = rangeMaxYear,
              value = c(rangeMinYear, rangeMaxYear),
              sep = '',
              step = 1L
            )
          )
        ),
        fluidRow(
          h2('HIV diagnoses, total'),
          wellPanel(
            sliderInput(
              ns('rangeHIVTotal'),
              label = NULL,
              min = rangeMinYear,
              max = rangeMaxYear,
              value = c(rangeMinYear, rangeMaxYear),
              sep = '',
              step = 1L
            )
          )
        ),
        fluidRow(
          h2('HIV diagnoses, by CD4 count'),
          wellPanel(
            sliderInput(
              ns('rangeHIVCD4'),
              label = NULL,
              min = rangeMinYear,
              max = rangeMaxYear,
              value = c(rangeMinYear, rangeMaxYear),
              sep = '',
              step = 1L
            )
          )
        ),
        fluidRow(
          h2('AIDS diagnoses, total'),
          wellPanel(
            sliderInput(
              ns('rangeAIDSTotal'),
              label = NULL,
              min = rangeMinYear,
              max = rangeMaxYear,
              value = c(rangeMinYear, rangeMaxYear),
              sep = '',
              step = 1L
            )
          )
        ),
        fluidRow(
          h2('HIV/AIDS diagnoses, total'),
          wellPanel(
            sliderInput(
              ns('rangeHIVAIDSTotal'),
              label = NULL,
              min = rangeMinYear,
              max = rangeMaxYear,
              value = c(rangeMinYear, rangeMaxYear),
              sep = '',
              step = 1L
            )
          )
        ),
        fluidRow(
          h2('Full/partial data'),
          wellPanel(
            fluidRow(
              column(
                width = 6,
                'Do you have data from the start of the epidemic'
              ),
              column(
                width = 6,
                radioButtons(ns('fullPartial'), label = NULL, choices = c('YES' = TRUE, 'NO' = FALSE))
              )
            )
          )
        ),
        fluidRow(
          h2('Incidence curve'),
          wellPanel(
            fluidRow(
              column(
                width = 6,
                'Knots count'
              ),
              column(
                width = 6,
                numericInput(ns('incidNumKnots'), label = NULL, value = 6)
              ),
              column(
                width = 6,
                'Start at zero'
              ),
              column(
                width = 6,
                checkboxInput(ns('incidStartZero'), label = NULL, value = TRUE)
              ),
              column(
                width = 6,
                'Prevent sudden changes at end of observation interval'
              ),
              column(
                width = 6,
                checkboxInput(ns('incidPreventChange'), label = NULL, value = TRUE)
              )
            )
          )
        ),
        fluidRow(
          h2('Maximum likelihood'),
          wellPanel(
            fluidRow(
              column(
                width = 6,
                'Distribution'
              ),
              column(
                width = 6,
                radioButtons(
                  ns('distr'),
                  label = NULL,
                  choices = c('Poisson' = 'POISSON', 'Negative Binomial' = 'NEGATIVE_BINOMIAL')
                )
              )
            )
          )
        ),
        fluidRow(
          h2('Diagnosis rate'),
          wellPanel(
            fluidRow(
              column(
                width = 6,
                'Extra rate due to non-AIDS symptoms'
              ),
              column(
                width = 6,
                numericInput(ns('extraDiagRate'), label = NULL, value =  0)
              )
            )
          )
        ),
        fluidRow(
          h2('Country specific settings'),
          wellPanel(
            fluidRow(
              column(
                width = 6,
                'Country'
              ),
              column(
                width = 6,
                selectizeInput(
                  ns('country'),
                  label = NULL,
                  choices = c('NL' = 'NL', 'Other' = 'OTHER')
                )
              )
            )
          )
        )
      )
    )
  )

}

# Server logic
paramsWidget <- function(input, output, session, appStatus, parentState)
{
  # Get namespace
  ns <- session$ns

  localState <- reactiveValues(
    Params = NULL
  )

  observeEvent(parentState$Context$Parameters$INCIDENCE, {
    localState$Params <- parentState$Context$Parameters$INCIDENCE
  })

  observeEvent({
    localState$Params$ModelMinYear
    localState$Params$ModelMaxYear
  }, {
    range <- c(localState$Params$ModelMinYear, localState$Params$ModelMaxYear)
    updateSliderInput(session, 'rangeMain', value = range)
  })

  observeEvent(input$rangeMain, {
    range <- input$rangeMain
    localState$Params$ModelMinYear <- range[1]
    localState$Params$ModelMaxYear <- range[2]
  })

  observeEvent({
    localState$Params$FitPosMinYear
    localState$Params$FitPosMaxYear
  }, {
    range <- c(localState$Params$FitPosMinYear, localState$Params$FitPosMaxYear)
    updateSliderInput(session, 'rangeHIVTotal', value = range)
  })

  observeEvent(input$rangeHIVTotal, {
    range <- input$rangeHIVTotal
    localState$Params$FitPosMinYear <- range[1]
    localState$Params$FitPosMaxYear <- range[2]
  })

  observeEvent({
    localState$Params$FitPosCD4MinYear
    localState$Params$FitPosCD4MaxYear
  }, {
    range <- c(localState$Params$FitPosCD4MinYear, localState$Params$FitPosCD4MaxYear)
    updateSliderInput(session, 'rangeHIVCD4', value = range)
  })

  observeEvent(input$rangeHIVCD4, {
    range <- input$rangeHIVCD4
    localState$Params$FitPosCD4MinYear <- range[1]
    localState$Params$FitPosCD4MaxYear <- range[2]
  })

  observeEvent({
    localState$Params$FitAIDSMinYear
    localState$Params$FitAIDSMaxYear
  }, {
    range <- c(localState$Params$FitAIDSMinYear, localState$Params$FitAIDSMaxYear)
    updateSliderInput(session, 'rangeAIDSTotal', value = range)
  })

  observeEvent(input$rangeAIDSTotal, {
    range <- input$rangeAIDSTotal
    localState$Params$FitAIDSMinYear <- range[1]
    localState$Params$FitAIDSMaxYear <- range[2]
  })

  observeEvent({
    localState$Params$FitAIDSPosMinYear
    localState$Params$FitAIDSPosMaxYear
  }, {
    range <- c(localState$Params$FitAIDSPosMinYear, localState$Params$FitAIDSPosMaxYear)
    updateSliderInput(session, 'rangeHIVAIDSTotal', value = range)
  })

  observeEvent(input$rangeHIVAIDSTotal, {
    range <- input$rangeHIVAIDSTotal
    localState$Params$FitAIDSPosMinYear <- range[1]
    localState$Params$FitAIDSPosMaxYear <- range[2]
  })

  observeEvent(localState$Params$FullData, {
    updateRadioButtons(session, 'fullPartial', selected = localState$Params$FullData)
  })

  observeEvent(input$fullPartial, {
    localState$Params$FullData <- as.logical(input$fullPartial)
  })

  observeEvent(localState$Params$ModelNoKnots, {
    updateNumericInput(session, 'incidNumKnots', value = localState$Params$ModelNoKnots)
  })

  observeEvent(input$incidNumKnots, {
    localState$Params$ModelNoKnots <- as.numeric(input$incidNumKnots)
  })

  observeEvent(localState$Params$StartIncZero, {
    updateCheckboxInput(session, 'incidStartZero', value = localState$Params$StartIncZero)
  })

  observeEvent(input$incidStartZero, {
    localState$Params$StartIncZero <- as.logical(input$incidStartZero)
  })

  observeEvent(localState$Params$MaxIncCorr, {
    updateCheckboxInput(session, 'incidPreventChange', value = localState$Params$MaxIncCorr)
  })

  observeEvent(input$incidPreventChange, {
    localState$Params$MaxIncCorr <- as.logical(input$incidStartZero)
  })

  observeEvent(localState$Params$FitDistribution, {
    updateRadioButtons(session, 'distr', selected = localState$Params$FitDistribution)
  })

  observeEvent(input$distr, {
    localState$Params$FitDistribution <- input$distr
  })

  observeEvent(localState$Params$Delta4Fac, {
    updateNumericInput(session, 'extraDiagRate', value = localState$Params$Delta4Fac)
  })

  observeEvent(input$extraDiagRate, {
    localState$Params$Delta4Fac <- as.numeric(input$extraDiagRate)
  })

  observeEvent(localState$Params$Country, {
    updateSelectInput(session, 'country', selected = localState$Params$Country)
  })

  observeEvent(input$country, {
    localState$Params$Country <- input$country
  })

  return(NULL)
}
