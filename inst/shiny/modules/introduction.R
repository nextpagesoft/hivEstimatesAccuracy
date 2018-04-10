# Module globals
# NONE

# User interface
introductionUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "Introduction",
      solidHeader = FALSE,
      collapsible = TRUE,
      status = "primary")
  )
}

# Server logic
introduction <- function(input, output, session)
{

}
