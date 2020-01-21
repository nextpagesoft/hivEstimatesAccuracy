# Module globals
# NONE

# User interface
welcomeUI <- function(id)
{
  ns <- NS(id)

  tagList(
    fluidRow(
      style = "margin-top: 20px",
      column(
        width = 3, offset = 1, style = "text-align: center",
        actionLink(ns("accuracy"), label = "HIV Accuracy", icon("calculator"))
      ),
      column(
        width = 3, style = "text-align: center",
        actionLink(ns("modelling"), label = "HIV Modelling", icon("bolt"))
      ),
      column(
        width = 3, style = "text-align: center",
        actionLink(ns("full"), label = "HIV Accuracy and Modelling", icon("blender"))
      )
    )
  )
}

# Server logic
welcome <- function(input, output, session, appStatus)
{
  observeEvent(input[["accuracy"]], {
    appStatus$Mode <- "ACCURACY"
  })

  observeEvent(input[["modelling"]], {
    appStatus$Mode <- "MODELLING"
  })

  observeEvent(input[["full"]], {
    appStatus$Mode <- "FULL"
  })
}
