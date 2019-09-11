# Module globals
# NONE

# User interface
hivModelUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = 'margin-top: 15px'),
    box(
      width = 12,
      title = 'HIV Modelling',
      solidHeader = FALSE,
      collapsible = TRUE,
      status = 'primary',
      fluidRow(
        column(
          width = 4,
          fileInput(ns('fileInput'), width = '100%', label = 'File input:'),
          p('Maximum file size: 70MB', tags$br(), 'Supported files types: zip')
        ),
        column(
          width = 8,
          withSpinner(uiOutput(ns('dataDetails')), type = 7, proxy.height = '50px')
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns('runBox'),
        box(
          width = 12,
          title = 'Run',
          solidHeader = FALSE,
          status = 'warning',
          collapsible = TRUE,
          actionButton(
            ns('runBtn'),
            label = 'Run',
            style = 'background-color: #69b023; color: white'
          ),
          shinyjs::disabled(
            actionButton(
              ns('cancelBtn'),
              label = 'Cancel'
            )
          )
        )
      )
    ),
    uiOutput(ns('log'))
  )
}

# Server logic
hivModel <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  # Make "task" behave like a reactive value
  makeReactiveBinding("task")
  task <- NULL

  # Local state
  localState <- reactiveValues(
    Context = NULL,
    Data = NULL,
    MainResults = NULL,
    Log = NULL
  )

  # Show/hide sections
  observe({
    data <- localState$Data
    if (is.null(data)) {
      shinyjs::hide('runBox', anim = TRUE, animType = 'fade')
    } else {
      shinyjs::show('runBox')
    }
  })

  # EVENT: Input data file name changed
  observeEvent(input[['fileInput']], {
    # Get reference to file input
    fileInput <- input$fileInput

    # Validate
    validate(need(fileInput, message = FALSE))

    context <- hivModelling::GetRunContext(
      settings = list(
        ModelsToRun = c('INCIDENCE'),
        InputDataPath = fileInput$datapath
      ),
      parameters = list(
        Models = list(
          INCIDENCE = list(
            Country = 'NL'
          )
        )
      )
    )

    data <- hivModelling::ReadInputData(context)

    localState$Context <- context
    localState$Data <- data
  })

  # Output data details when they have changed
  output[['dataDetails']] <- renderUI({
    data <- req(localState$Data)

    isolate({
      # Get reference to file input
      fileInput <- input$fileInput

      # Get input data details
      dataDetails <-
        fluidRow(
          column(
            width = 3,
            p(strong('Name:'), fileInput$name),
            p(strong('Size:'), FormatObjectSize(fileInput$size)),
            p(strong('Type:'), fileInput$type),
            p(strong('Number of files:'), length(data))
          ),
          column(
            width = 9,
            p(strong('File names:')),
            p(paste(names(data), collapse = ', ')))
        )

      return(dataDetails)
    })
  })

  # EVENT: Button "Run" clicked
  observeEvent(input[['runBtn']], {
    shinyjs::disable('runBtn')
    shinyjs::enable('cancelBtn')

    context <- req(localState$Context)
    data <- req(localState$Data)
    localState$MainResults <- NULL
    localState$Log <- ''

    # Show progress message during task start
    prog <- Progress$new(session)
    prog$set(message = 'Performing main fit...', value = 0.1)

    startTime <- Sys.time()
    if (isLinux) {
      task <<- CreateTask({
        hivModelling::PerformMainFit(context, data)
      })
    } else {
      task <<- CreateTask(function(context, data) {
        hivModelling::PerformMainFit(context, data)
      },
      args = list(context, data))
    }

    o <- observe({
      # Only proceed when the task is completed (this could mean success,
      # failure, or cancellation)
      req(task$completed())
      endTime <- Sys.time()

      mainResults <- task$result()
      task <<- NULL
      if (is.list(mainResults)) {
        localState$MainResults <- mainResults
      } else {
        localState$MainResults <- NULL
        localState$Log <- 'Main fit cancelled'
      }
      localState$Log <- paste(
        paste('Start time  :', FormatTime(startTime)),
        paste('End time    :', FormatTime(endTime)),
        paste('Elapsed time:', FormatDiffTime(endTime - startTime)),
        paste(''),
        localState$Log,
        sep = '\n'
      )

      # This observer only runs once
      o$destroy()

      # Close the progress indicator and update button state
      prog$close()
      shinyjs::enable('runBtn')
      shinyjs::disable('cancelBtn')
    })
  })

  # EVENT: Button "Cancel" clicked
  observeEvent(input[['cancelBtn']], {
    req(task)$cancel()
  })

  # Populate adjustment parameter widgets in the editing dialog
  output[['log']] <- renderUI({
    runLogHTML <- box(
      width = 12,
      title = 'Log',
      solidHeader = FALSE,
      collapsible = TRUE,
      status = 'warning',
      tags$pre(req(localState$Log))
    )
    return(runLogHTML)
  })

  return(NULL)
}
