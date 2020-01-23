# Module globals
# NONE

# User interface
welcomeUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    div(
      id = ns("introductionBox"),
      box(
        width = 12,
        fluidRow(
          column(
            offset = 3,
            width = 6,
            style = "text-align: center; font-weight: bold",
            p("The ECDC HIV Estimates Accuracy Tool is an application that uses advanced statistical methods to correct for missing values in key HIV surveillance variables as well as for reporting delay, as defined by the time from case diagnosis to notification at the national level."),
            p("The tool accepts case based HIV surveillance data prepared in a specific format."),
            p("The outputs include results from pre-defined analyses in the form of a report containing tables and graphs, and datasets, in which the adjustments have been incorporated and which may be exported for further analysis.",
              style = "margin-bottom: 0")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 3, offset = 1, style = "text-align: center",
        actionLink(ns("modelling"), label = "Modelling", icon("bolt"))
      ),
      column(
        width = 3, style = "text-align: center",
        actionLink(ns("accuracy"), label = "Accuracy", icon("calculator"))
      ),
      column(
        width = 3, style = "text-align: center",
        actionLink(ns("full"), label = "Both", icon("blender"))
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
