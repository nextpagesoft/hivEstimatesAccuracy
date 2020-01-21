# Module globals
# NONE

# User interface
welcomeUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "Welcome",
      solidHeader = FALSE,
      collapsible = TRUE,
      status = "primary",
      actionButton(
        ns("accuracy"),
        label = "HIV Accuracy"
      ),
      actionButton(
        ns("modelling"),
        label = "HIV Modelling"
      ),
      actionButton(
        ns("full"),
        label = "HIV Accuracy and Modelling"
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
