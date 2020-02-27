groupingNames <-
  c('REPCOUNTRY + UNK + OTHER',
    'REPCOUNTRY + UNK + SUBAFR + OTHER',
    'REPCOUNTRY + UNK + 3 most prevalent regions + OTHER',
    'Custom')

GetGroupingTable <- function(type, distr, map) {
  groupDistr <- merge(distr, map, all.x = TRUE)
  groupDistr <- groupDistr[, .(
    FullRegionOfOrigin = paste(FullRegionOfOrigin, collapse = ', '),
    Count = sum(Count)
    ), by = .(GroupedRegionOfOrigin = as.character(GroupedRegionOfOrigin))]
  groupDistr <- groupDistr[order(-Count)]
  groupDistr <- rbind(groupDistr[GroupedRegionOfOrigin == 'REPCOUNTRY'],
                      groupDistr[!GroupedRegionOfOrigin %chin% c('REPCOUNTRY', 'UNK', 'OTHER')],
                      groupDistr[GroupedRegionOfOrigin == 'OTHER'],
                      groupDistr[GroupedRegionOfOrigin == 'UNK'])
  return(groupDistr)
}

# User interface
caseBasedMigrantWidgetUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(
      id = ns('migrantBox'),
      box(
        width = 12,
        title = 'Migrant variable regrouping',
        solidHeader = FALSE,
        status = 'warning',
        collapsible = TRUE,
        fluidRow(
          column(
            3,
            p(HTML('Distribution of region of origin:<br /><small>All regions in dataset in descending frequency</small>')),
            tableOutput(ns('regionOfOriginTable'))
          ),
          column(
            9,
            selectInput(
              ns('groupSelect'),
              width = '400px',
              label = 'Grouping options',
              choices = groupingNames,
              selected = 0,
              selectize = TRUE
            ),
            tableOutput(ns('groupingTableDT')),
            shinyjs::hidden(uiOutput(ns('groupingTableUI'))),
            uiOutput(ns('testDiv'))
          )
        )
      )
    )
  )
}

# Server logic
caseBasedMigrantWidget <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  localState <- reactiveValues(
    lastGroupWidgetIndex = 0L,
    availableRegions = c(),
    groupNames = list(),
    groupRegions = list(),
    groupCounts = list(),
    map = NULL
  )

  # Get widget for creating a group
  GetGroupCreateWidget <- function() {
    # Get unique index for the elements
    index <- localState$lastGroupWidgetIndex + 1
    key <- as.character(index)
    initGroupName <- paste('GROUP', key)

    distr <- distr()
    rowId          <- paste0('row', key)
    deleteBtnId    <- paste0('deleteBtn', key)
    groupNameId    <- paste0('groupName', key)
    regionSelectId <- paste0('regionSelect', key)
    distCountId    <- paste0('distrCount', key)

    allSelectedRegions <- Reduce(union, localState$groupRegions, c())

    localState$groupNames[[key]] <- initGroupName
    localState$groupRegions[[key]] <- c()
    localState$lastGroupWidgetIndex <- index

    # Get widget html
    widget <- tags$tr(
      id = ns(rowId),
      tags$td(actionLink(ns(deleteBtnId), label = 'Remove')),
      tags$td(textInput(ns(groupNameId), label = NULL, value = initGroupName)),
      # Selection input
      tags$td(selectInput(
        ns(regionSelectId),
        label = NULL,
        choices = setdiff(localState$availableRegions, allSelectedRegions),
        multiple = TRUE)
      ),
      tags$td(textOutput(ns(distCountId), inline = TRUE))
    )

    # EVENT: Button 'Remove' clicked
    observeEvent(input[[deleteBtnId]], {
      localState$groupNames[[key]] <- NULL
      localState$groupRegions[[key]] <- NULL
      localState$groupCounts[[key]] <- NULL
      removeUI(selector = paste0('#', ns(rowId)), session = session)
    })

    # EVENT: Group name edited
    observeEvent(input[[groupNameId]], {
      localState$groupNames[[key]] <- input[[groupNameId]]
    })

    # EVENT: Regions selected
    observeEvent(input[[regionSelectId]], {
      localState$groupRegions[[key]] <- input[[regionSelectId]]
      localState$groupCounts[[key]] <-
        distr[FullRegionOfOrigin %in% localState$groupRegions[[key]], sum(Count)]

      allSelectedRegions <- Reduce(union, localState$groupRegions, c())
      groupKeys <- setdiff(names(localState$groupNames), key)
      for (groupKey in groupKeys) {
        id <- paste0('regionSelect', groupKey)
        selectedRegions <- input[[id]]
        updateSelectInput(
          session,
          id,
          choices = union(setdiff(localState$availableRegions, allSelectedRegions), selectedRegions),
          selected = selectedRegions
        )
      }
    })

    output[[distCountId]] <- renderText({
      localState$groupCounts[[key]]
    })

    return(widget)
  }

  distr <- reactive({
    inputData <- req(appStatus$InputDataBeforeGrouping$Table)
    GetOriginDistribution(inputData)
  })

  output[['regionOfOriginTable']] <- renderTable({
    req(distr())
  })

  observe({
    type <- req(input[['groupSelect']])
    distr <- req(distr())
    groupRegions <- req(localState$groupRegions)
    groupNames <- req(localState$groupNames)
    req(length(groupRegions) == length(groupNames))
    groups <- list()
    for (key in names(groupNames)) {
      groups[[length(groups) + 1]] <- list(Name = groupNames[[key]], Regions = groupRegions[[key]])
    }
    localState$map <- GetOriginGroupingMap(type, distr, groups)
  })

  observe({
    localState$availableRegions <- req(distr())$FullRegionOfOrigin
  })

  observe({
    type <- input$groupSelect
    if (type %in% groupingNames[1:3]) {
      shinyjs::hide('groupingTableUI')
      shinyjs::show('groupingTableDT')
    } else {
      shinyjs::hide('groupingTableDT')
      shinyjs::show('groupingTableUI')
    }
  })

  output[['groupingTableDT']] <- renderTable({
    type <- req(input$groupSelect)
    req(type %in% groupingNames[1:3])
    distr <- req(distr())
    map <- req(localState$map)
    GetGroupingTable(type, distr, map)
  }, width = '100%')

  output[['groupingTableUI']] <- renderUI({
    tagList(
      tags$table(
        class = 'table shiny-table table- spacing-s',
        style = 'width:100%;',
        tags$thead(
          tags$th(),
          tags$th(
            style = 'text-align: left;',
            ' GroupedRegionOfOrigin '
          ),
          tags$th(
            style = 'text-align: left;',
            ' FullRegionOfOrigin '
          ),
          tags$th(
            style = 'text-align: right;',
            ' Count '
          )
        ),
        tags$tbody(
          id = ns('groupsList')
        )
      ),
      actionButton(ns('addGroupBtn'), 'Add group')
    )
  })

  # Add adjustment selection widget
  observeEvent(input[['addGroupBtn']], {
    insertUI(
      selector = paste0('#', ns('groupsList')),
      where = 'beforeEnd',
      ui = GetGroupCreateWidget(),
      session = session
    )
  })

  observe({
    inputData <- req(appStatus$InputDataBeforeGrouping)
    map <- req(localState$map)
    inputData <- ApplyOriginGroupingMap(inputData, map)
    appStatus$InputData <- inputData
    appStatus$AdjustedData <- NULL
    appStatus$HIVModelData <- PrepareDataSetsForModel(
      inputData$Table,
      splitBy = 'Imputation',
      strata = c('Gender', 'Transmission'),
      listIndex = 0
    )
  })

  return(NULL)
}
