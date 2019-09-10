# Module globals
# NONE

# User interface
hivModelUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "HIV Modelling",
      solidHeader = FALSE,
      collapsible = TRUE,
      status = "primary",
      p('Test')
    )
  )
}

# Server logic
hivModel <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns
}
