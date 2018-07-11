# Module globals
# NONE

# User interface
manualUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "Manual",
      solidHeader = FALSE,
      collapsible = TRUE,
      status = "primary",
      p("This is a placeholder for the manual.")
    )
  )

}

# Server logic
manual <- function(input, output, session)
{

}
