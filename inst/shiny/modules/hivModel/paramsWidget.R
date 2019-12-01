# Module globals
rangeMinYear <- 1975L
rangeMaxYear <- 2020L
minYear <- 1980L
maxYear <- 2016L

# User interface
paramsWidgetUI <- function(id)
{
  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      h1('3. Parameters'),
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
              value = c(minYear, maxYear)
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
              value = c(minYear, maxYear)
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
              value = c(minYear, maxYear)
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
              value = c(minYear, maxYear)
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
              value = c(minYear, maxYear)
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
          h2('Confidence intervals'),
          wellPanel(
            fluidRow(
              column(
                width = 6,
                'Number of iterations'
              ),
              column(
                width = 6,
                numericInput(ns('numIter'), label = NULL, value = 20)
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
paramsWidget <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  return(NULL)
}
