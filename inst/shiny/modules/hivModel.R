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
      fluidRow(
        column(width = 4,
               fileInput(ns("fileInput"),
                         width = "100%",
                         label = "File input:"),
               p("Maximum file size: 70MB",
                 tags$br(),
                 "Supported files types: zip")
        )
      )
    )
  )
}

# Server logic
hivModel <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  localStatus <- reactiveValues(
    InputFolderName = NULL
  )

  # EVENT: Input data file name changed
  observeEvent(input[["fileInput"]], {
    # Get reference to file input
    fileInput <- input$fileInput

    # Validate
    validate(need(fileInput, message = FALSE))

    # Read input data
    inputFolderName <- req(ReadDataFile(fileInput$datapath))
    localStatus$InputFolderName <- TRUE
  })
}
