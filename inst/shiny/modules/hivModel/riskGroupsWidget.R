# User interface
riskGroupsWidgetUI <- function(id)
{
  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      h1('1. Risk groups')
    )
  )

}

# Server logic
riskGroupsWidget <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  localState <- reactiveValues()

  return(NULL)
}
