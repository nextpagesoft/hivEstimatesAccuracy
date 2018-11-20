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
      tags$iframe(style = "height:900px; width:100%; border: none",
                  src = "pdf/HIV_Estimates_Accuracy_manual.pdf#zoom=100")
    )
  )

}

# Server logic
manual <- function(input, output, session)
{

}
