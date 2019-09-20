# Module globals
minYear <- 1980L
maxYear <- 2016L

initInterval <- data.table(
  StartYear = minYear,
  EndYear = maxYear,
  Jump = FALSE,
  DiffByCD4 = FALSE,
  ChangeInInterval = FALSE
)

# User interface
diagRateWidgetUI <- function(id)
{
  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      h1('1. Diagnosis rates')
    ),
    column(
      width = 6,
      h2('Intervals list'),
      wellPanel(
        dataTableOutput(ns('tableDT'))
      )
    ),
    column(
      width = 6,
      uiOutput(ns('editIntervalUI'))
    )
  )

}

# Server logic
diagRateWidget <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  localState <- reactiveValues(
    Intervals = initInterval,
    SelectedIntervalIdx = NULL,
    EditMode = NULL
  )

  tableProxy <- dataTableProxy(ns('tableDT'))

  output[['tableDT']] <- renderDataTable({
    intervals <- copy(localState$Intervals)
    datatable(
      intervals,
      style = 'default',
      colnames = c('Start year', 'End year', 'Jump', 'Change by CD4 count', 'Change in interval'),
      class = 'table-bordered table-condensed compact nowrap hover',
      editable = FALSE,
      selection = list(mode = 'single', selected = localState$SelectedIntervalIdx),
      options = list(
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(
            targets = c(3:5),
            render = JS(
              'function(data) {',
              '  return \'<input type="checkbox"\' + (data ? \' checked \' : \' \') + \'onclick="return false;">\';',
              '}'
            )
          )
        )
      )
    )
  }, server = FALSE)

  observeEvent(input[['tableDT_rows_selected']], {
    localState$SelectedIntervalIdx <- input[['tableDT_rows_selected']]
  }, ignoreNULL = FALSE)

  output[['editIntervalUI']] <- renderUI({

    idx <- localState$SelectedIntervalIdx
    intervals <- localState$Intervals

    applyChangesBtn <- actionButton(
      ns('applyChangesBtn'),
      label = 'Apply changes',
      style = 'background-color: #69b023; color: white'
    )

    addNewBtn <- actionButton(
      ns('addNewBtn'),
      label = 'Add new',
      style = 'background-color: #69b023; color: white'
    )

    deleteBtn <- actionButton(
      ns('deleteBtn'),
      label = 'Delete',
      style = 'background-color: #69b023; color: white'
    )

    if (!is.null(idx) && !is.null(intervals)) {
      localState$EditMode <- 'EDIT'
      title <- 'Edit interval'
      startYear <- intervals[idx, StartYear]
      jump <- intervals[idx, Jump]
      diffByCD4 <- intervals[idx, DiffByCD4]
      changeInInterval <- intervals[idx, ChangeInInterval]
      addNewBtn <- shinyjs::disabled(addNewBtn)
      if (nrow(intervals) < 2) {
        deleteBtn <- shinyjs::disabled(deleteBtn)
      }
    } else {
      localState$EditMode <- 'ADD'
      title <- 'Add interval'
      startYear <- ifelse(!is.null(intervals), intervals[, max(EndYear)], minYear)
      jump <- FALSE
      diffByCD4 <- FALSE
      changeInInterval <- FALSE
      applyChangesBtn <- shinyjs::disabled(applyChangesBtn)
      deleteBtn <- shinyjs::disabled(deleteBtn)
    }

    tagList(
      h2(title),
      wellPanel(
        fluidRow(
          column(
            width = 6,
            'Start year',
            style = 'text-align: right; margin-top: 8px'
          ),
          column(
            width = 3,
            numericInput(ns('startYear'), label = NULL, value = startYear, width = '100%')
          ),
          column(
            width = 3,
            textOutput(ns('startYearMsg'))
          )
        ),
        fluidRow(
          column(
            width = 6,
            'Jump',
            style = 'text-align: right; margin-top: 8px'
          ),
          column(
            width = 6,
            checkboxInput(ns('jump'), label = NULL, value = jump)
          )
        ),
        fluidRow(
          column(
            width = 6,
            'Different by CD4 count',
            style = 'text-align: right; margin-top: 8px'
          ),
          column(
            width = 6,
            checkboxInput(ns('diffByCD4'), label = NULL, value = diffByCD4)
          )
        ),
        fluidRow(
          column(
            width = 6,
            'Change in interval',
            style = 'text-align: right; margin-top: 8px'
          ),
          column(
            width = 6,
            checkboxInput(ns('changeInInterval'), label = NULL, value = changeInInterval)
          )
        ),
        fluidRow(
          column(
            style = 'text-align: center',
            width = 12,
            addNewBtn,
            applyChangesBtn,
            deleteBtn
          )
        )
      )
    )
  })

  output[['startYearMsg']] <- renderText({
    startYear <- input$startYear
    validate(
      need(startYear >= minYear, sprintf('Start year must be at least %d', minYear)),
      need(startYear <= maxYear, sprintf('Start year must be at most %d', maxYear))
    )
    ''
  })

  observeEvent(input[['addNewBtn']], {
    startYear <- input$startYear
    if (startYear %between% c(minYear, maxYear)) {
      interval <- data.table(
        StartYear = input$startYear,
        EndYear = NA_integer_,
        Jump = input$jump,
        DiffByCD4 = input$diffByCD4,
        ChangeInInterval = input$changeInInterval
      )

      localState$Intervals <- AddInterval(
        localState$Intervals,
        interval
      )
    }
  })

  observeEvent(input[['deleteBtn']], {
    idx <- req(localState$SelectedIntervalIdx)
    intervals <- req(localState$Intervals)

    intervals <- intervals[-idx]

    if (nrow(intervals) == 0) {
      intervals <- initInterval
    } else if (nrow(intervals) == 1) {
      intervals[, ':='(
        StartYear = minYear,
        EndYear = maxYear
      )]
    }

    localState$Intervals <- intervals
    localState$SelectedIntervalIdx <- nrow(intervals)
  })

  observeEvent(input[['applyChangesBtn']], {
    startYear <- input$startYear
    idx <- localState$SelectedIntervalIdx
    intervals <- localState$Intervals
    if (startYear %between% c(minYear, maxYear) && idx %between% c(1, nrow(intervals))) {
      interval <- data.table(
        StartYear = input$startYear,
        EndYear = NA_integer_,
        Jump = input$jump,
        DiffByCD4 = input$diffByCD4,
        ChangeInInterval = input$changeInInterval
      )

      localState$Intervals <- AddInterval(
        intervals,
        interval,
        idx
      )
    }
  })

  return(NULL)
}
