# Module globals
# NONE

# Load application modules
modulesPath <- system.file('shiny/modules/hivModel', package = 'hivEstimatesAccuracy')
source(file.path(modulesPath, 'riskGroupsWidget.R'))
source(file.path(modulesPath, 'diagRateWidget.R'))
source(file.path(modulesPath, 'paramsWidget.R'))

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
          fileInput(ns('fileInput'), width = '100%', label = 'XML Model parameters file input:')
        ),
        column(
          width = 8,
          withSpinner(uiOutput(ns('dataDetails')), type = 7, proxy.height = '50px')
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns('paramBox'),
        box(
          width = 12,
          title = 'Model',
          solidHeader = FALSE,
          collapsible = TRUE,
          status = 'primary',
          riskGroupsWidgetUI(ns('riskGroups')),
          h1('Incidence Method parameters'),
          tabsetPanel(
            type = 'tabs',
            tabPanel('Inputs', diagRateWidgetUI(ns('diagRate'))),
            tabPanel('Advanced', paramsWidgetUI(ns('params')))
          )
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
          div(
            actionButton(
              ns('runMainBtn'),
              label = 'Main fit',
              style = 'background-color: #69b023; color: white'
            ),
            shinyjs::disabled(
              actionButton(
                ns('cancelMainBtn'),
                label = 'Cancel'
              )
            )
          ),
          div(
            style = 'margin-top: 30px',
            numericInput(
              ns('bsNumIter'),
              label = 'Number of boostrap iterations',
              value = 20,
              min = 1,
              max = 100,
              step = 1,
              width = 200
            ),
            shinyjs::disabled(
              actionButton(
                ns('runBootstrapBtn'),
                label = 'Bootstrap fits',
                style = 'background-color: #69b023; color: white'
              )
            ),
            shinyjs::disabled(
              actionButton(
                ns('cancelBootstrapBtn'),
                label = 'Cancel'
              )
            )
          )
        )
      )
    ),
    uiOutput(ns('log')),
    uiOutput(ns('plots'))
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
    InputDataPath = NULL,
    Context = NULL,
    Data = NULL,
    PopData = NULL,
    MainResults = NULL,
    BSResultsList = NULL,
    Plots = NULL,
    Log = NULL
  )

  # Show/hide sections
  observe({
    data <- localState$Data
    if (is.null(data)) {
      shinyjs::hide('runBox', anim = TRUE, animType = 'fade')
      shinyjs::hide('paramBox', anim = TRUE, animType = 'fade')
    } else {
      shinyjs::show('runBox')
      shinyjs::show('paramBox')
    }
  })

  # EVENT: Input data file name changed
  observeEvent(input[['fileInput']], {
    # Validate
    validate(need(input$fileInput, message = FALSE))

    localState$InputDataPath <- input$fileInput$datapath
  })

  # EVENT: Input data file name changed
  observeEvent(localState$InputDataPath, {
    context <- hivModelling::GetRunContext(
      settings = list(
        InputDataPath = localState$InputDataPath
      )
    )

    data <- hivModelling::ReadInputData(context)
    popData <- hivModelling::GetPopulationData(context, data)

    localState$Context <- context
    localState$Data <- data
    localState$PopData <- popData
  })

  # Output data details when they have changed
  output[['dataDetails']] <- renderUI({
    data <- req(localState$Data)

    isolate({
      # Get reference to file input
      fileInput <- input$fileInput

      fileList <- setDT(unzip(input$fileInput$datapath, list = TRUE))
      dataFileNames <- fileList[Length > 0 & tools::file_ext(Name) == 'csv', basename(Name)]
      modelFileName <- fileList[Length > 0 & tools::file_ext(Name) == 'xml', basename(Name)][1]

      # Get input data details
      dataDetails <-
        fluidRow(
          column(
            width = 3,
            p(strong('Name:'), fileInput$name),
            p(strong('Size:'), FormatObjectSize(fileInput$size)),
            p(strong('Type:'), fileInput$type),
            p(strong('Number of files:'), nrow(fileList[Length > 0]))
          ),
          column(
            width = 9,
            p(strong('Model file name:')),
            p(modelFileName),
            p(strong('Data file names:')),
            p(paste(dataFileNames, collapse = ', '))
          )
        )

      return(dataDetails)
    })
  })

  observeEvent(input[['runMainBtn']], {
    shinyjs::disable('runMainBtn')
    shinyjs::enable('cancelMainBtn')
    shinyjs::disable('runBootstrapBtn')
    shinyjs::disable('cancelBootstrapBtn')

    context <- req(localState$Context)
    popData <- req(localState$PopData)
    localState$MainResults <- NULL
    localState$BSResultsList <- NULL
    localState$Plots <- NULL
    localState$Log <- ''

    # Show progress message during task start
    prog <- Progress$new(session)
    prog$set(message = 'Performing main fit...', value = 0.1)

    startTime <- Sys.time()
    if (isLinux) {
      task <<- CreateTask({
        hivModelling::PerformMainFit(context, popData, maxNoFit = 20, ctol = 1e-5, ftol = 1e-4)
      })
    } else {
      task <<- CreateTask(function(context, popData) {
        hivModelling::PerformMainFit(context, popData, maxNoFit = 20, ctol = 1e-5, ftol = 1e-4)
      },
      args = list(context, popData))
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
        paste('* Main fit'),
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
      shinyjs::enable('runMainBtn')
      shinyjs::disable('cancelMainBtn')
    })
  })

  observeEvent(input[['cancelMainBtn']], {
    req(task)$cancel()
  })

  observeEvent(localState$MainResults, {
    shinyjs::enable('runBootstrapBtn')
    shinyjs::disable('cancelBootstrapBtn')
    mainResults <- localState$MainResults
    bsResultsList <- localState$BSResultsList
    plots <- hivModelling::CreateOutputPlots(mainResults, bsResultsList)
    localState$Plots <- plots
  })

  observeEvent(input[['runBootstrapBtn']], {
    shinyjs::disable('runMainBtn')
    shinyjs::disable('cancelMainBtn')
    shinyjs::disable('runBootstrapBtn')
    shinyjs::enable('cancelBootstrapBtn')

    context <- req(localState$Context)
    popData <- req(localState$PopData)
    mainResults <- req(localState$MainResults)
    bsCount <- req(input$bsNumIter)

    localState$BSResultsList <- NULL

    # Show progress message during task start
    prog <- Progress$new(session)
    prog$set(message = 'Performing boostrap fits...', value = 0.1)

    startTime <- Sys.time()
    if (isLinux) {
      task <<- CreateTask({
        hivModelling::PerformBootstrapFits(
          context,
          popData,
          mainResults,
          bsCount,
          executionPlan = future::sequential
        )
      })
    } else {
      task <<- CreateTask(function(context, popData, mainResults, bsCount) {
        hivModelling::PerformBootstrapFits(
          context,
          popData,
          mainResults,
          bsCount,
          executionPlan = future::sequential
        )
      },
      args = list(context, popData, mainResults, bsCount))
    }

    o <- observe({
      # Only proceed when the task is completed (this could mean success,
      # failure, or cancellation)
      req(task$completed())
      endTime <- Sys.time()

      bsResultsList <- task$result()
      task <<- NULL
      if (is.list(mainResults)) {
        localState$BSResultsList <- bsResultsList
        msg <- ''
      } else {
        localState$BSResultsList <- NULL
        msg <- 'Bootstrap fits cancelled'
      }
      localState$Log <- paste(
        localState$Log,
        paste(''),
        paste('* Bootstrap fits'),
        paste('Start time  :', FormatTime(startTime)),
        paste('End time    :', FormatTime(endTime)),
        paste('Elapsed time:', FormatDiffTime(endTime - startTime)),
        msg,
        sep = '\n'
      )

      # This observer runs only once
      o$destroy()

      # Close the progress indicator and update button state
      prog$close()
      shinyjs::enable('runBootstrapBtn')
      shinyjs::disable('cancelBootstrapBtn')
    })
  })

  observeEvent(input[['cancelBootstrapBtn']], {
    req(task)$cancel()
  })

  observeEvent(localState$BSResultsList, {
    mainResults <- localState$MainResults
    bsResultsList <- localState$BSResultsList
    plots <- hivModelling::CreateOutputPlots(mainResults, bsResultsList)
    localState$Plots <- plots
  })

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

  output[['plots']] <- renderUI({
    req(localState$Plots)
    plotsHTML <- box(
      width = 12,
      title = 'Plots',
      solidHeader = FALSE,
      collapsible = TRUE,
      status = 'warning',
      plotOutput(ns('N_HIV_Obs_M')),
      plotOutput(ns('N_CD4_1_Obs_M')),
      plotOutput(ns('N_CD4_2_Obs_M')),
      plotOutput(ns('N_CD4_3_Obs_M')),
      plotOutput(ns('N_CD4_4_Obs_M')),
      plotOutput(ns('N_HIVAIDS_Obs_M')),
      plotOutput(ns('N_AIDS_M')),
      plotOutput(ns('N_Inf_M')),
      plotOutput(ns('t_diag')),
      plotOutput(ns('N_Alive')),
      plotOutput(ns('N_Und_Alive_p'))
    )
    return(plotsHTML)
  })

  output[['N_HIV_Obs_M']] <- renderPlot({
    localState$Plots[['HIV diagnoses, total']]
  })

  output[['N_CD4_1_Obs_M']] <- renderPlot({
    localState$Plots[['HIV diagnoses, CD4 >= 500']]
  })

  output[['N_CD4_2_Obs_M']] <- renderPlot({
    localState$Plots[['HIV diagnoses, CD4 >= 350-499']]
  })

  output[['N_CD4_3_Obs_M']] <- renderPlot({
    localState$Plots[['HIV diagnoses, CD4 >= 200-349']]
  })

  output[['N_CD4_4_Obs_M']] <- renderPlot({
    localState$Plots[['HIV diagnoses, CD4 < 200']]
  })

  output[['N_HIVAIDS_Obs_M']] <- renderPlot({
    localState$Plots[['HIV/AIDS diagnoses']]
  })

  output[['N_AIDS_M']] <- renderPlot({
    localState$Plots[['AIDS diagnoses, total']]
  })

  output[['N_Inf_M']] <- renderPlot({
    localState$Plots[['HIV infections per year']]
  })

  output[['t_diag']] <- renderPlot({
    localState$Plots[['Time to diagnosis']]
  })

  output[['N_Alive']] <- renderPlot({
    localState$Plots[['Total number of HIV-infected']]
  })

  output[['N_Und_Alive_p']] <- renderPlot({
    localState$Plots[['Proportion undiagnosed of all those alive']]
  })

  callModule(riskGroupsWidget, 'riskGroups', appStatus, localState)
  callModule(diagRateWidget, 'diagRate', appStatus, localState)
  callModule(paramsWidget, 'params', appStatus, localState)

  return(NULL)
}
