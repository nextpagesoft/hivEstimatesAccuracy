# User interface
riskGroupsWidgetUI <- function(id)
{
  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      h1('Populations combinations')
    ),
    column(
      width = 6,
      wellPanel(
        fluidRow(
          column(
            width = 4,
            selectInput(
              ns('riskGroupSelect'),
              label = 'Select populations combination',
              choices = c(),
              selectize = TRUE
            )
          ),
          column(
            width = 4,
            textInput(
              ns('riskGroupName'),
              label = 'Edit name'
            )
          ),
          column(
            width = 4,
            checkboxGroupInput(
              ns('populationSelect'),
              label = 'Select populations for combination',
              choices = c()
            )
          )
        )
      )
    )
  )

}

# Server logic
riskGroupsWidget <- function(input, output, session, appStatus, parentState)
{
  # Get namespace
  ns <- session$ns

  observeEvent(parentState$Context$Settings$RiskGroups, {
    riskGroupNames <- names(parentState$Context$Settings$RiskGroups$PopulationSets)
    selRiskGroup <- parentState$Context$Settings$RiskGroups$Selected
    updateSelectInput(session, 'riskGroupSelect', choices = riskGroupNames, selected = selRiskGroup)

    updateTextInput(session, 'riskGroupName', value = selRiskGroup)

    allPopulations <- names(parentState$Data)
    selPopulations <- parentState$Context$Settings$RiskGroups$PopulationSets[[selRiskGroup]]
    updateCheckboxGroupInput(session, 'populationSelect', choices = allPopulations,
                             selected = selPopulations)
  })

  observeEvent(input$riskGroupSelect, {
    parentState$Context$Settings$RiskGroups$Selected <- input$riskGroupSelect
  })

  observeEvent(input$riskGroupName, {
    newName <- input$riskGroupName
    if (nchar(newName) == 0) {
      return(NULL)
    }
    oldNames <- names(parentState$Context$Settings$RiskGroups$PopulationSets)
    oldName <- parentState$Context$Settings$RiskGroups$Selected
    idx <- which(oldNames == oldName)

    newNames <- oldNames
    newNames[idx] <- newName
    names(parentState$Context$Settings$RiskGroups$PopulationSets) <- newNames
    parentState$Context$Settings$RiskGroups$Selected <- newName
  })

  observeEvent(input$populationSelect, {
    selPopulations <- input$populationSelect
    if (is.null(selPopulations)) {
      selPopulations <- character()
    }

    selRiskGroup <- parentState$Context$Settings$RiskGroups$Selected
    parentState$Context$Settings$RiskGroups$PopulationSets[[selRiskGroup]] <- selPopulations
  }, ignoreNULL = FALSE)


  return(NULL)
}
