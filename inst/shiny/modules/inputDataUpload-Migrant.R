groupingNames <-
  c("REPCOUNTRY + UNK + OTHER",
    "REPCOUNTRY + UNK + SUBAFR + OTHER",
    "REPCOUNTRY + UNK + 4 most prevalent other regions",
    "Custom")

GetGroupingTable <- function(type, distr, map) {
  groupDistr <- merge(distr, map, all.x = TRUE)
  groupDistr <- groupDistr[, .(
    FullRegionOfOrigin = paste(FullRegionOfOrigin, collapse = ", "),
    Count = sum(Count)
    ), by = GroupedRegionOfOrigin]
  groupDistr <- groupDistr[order(-Count)]
  groupDistr <- rbind(groupDistr[GroupedRegionOfOrigin == "REPCOUNTRY"],
                      groupDistr[!GroupedRegionOfOrigin %chin% c("REPCOUNTRY", "UNK", "OTHER")],
                      groupDistr[GroupedRegionOfOrigin == "OTHER"],
                      groupDistr[GroupedRegionOfOrigin == "UNK"])
  return(groupDistr)
}

# User interface
inputDataUploadMigrantUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(
      id = ns("migrantBox"),
      box(
        width = 12,
        title = "Migrant variable regrouping",
        solidHeader = FALSE,
        status = "warning",
        collapsible = TRUE,
        fluidRow(
          column(
            4,
            p(HTML("Distribution of region of origin:<br /><small>All regions in dataset in descending frequency</small>")),
            tableOutput(ns("regionOfOriginTable"))
          ),
          column(
            8,
            selectInput(ns("groupSelect"),
                        width = "400px",
                        label = "Grouping options",
                        choices = groupingNames,
                        selected = 0,
                        selectize = TRUE),
            uiOutput(ns("groupingTableDiv")),
            uiOutput(ns("testDiv"))
          )
        )
      )
    )
  )
}

# Server logic
inputDataUploadMigrant <- function(input, output, session, inputData)
{
  # Get namespace
  ns <- session$ns

  vals <- reactiveValues(lastGroupWidgetIndex = 0L,
                         groupMaps = list())

  # Get widget for creating a group
  GetGroupCreateWidget <- function() {
    distr <- req(distr())
    # Get unique index for the elements
    index <- vals$lastGroupWidgetIndex + 1
    key <- as.character(index)

    rowId          <- paste0("row", key)
    deleteBtnId    <- paste0("deleteBtn", key)
    groupNameId    <- paste0("groupName", key)
    regionSelectId <- paste0("adjustSelect", key)
    distCountId    <- paste0("distrCount", key)
    groupName      <- ""
    groupRegions   <- c()

    # Get widget html
    widget <- fluidRow(
      id = ns(rowId),
      column(1, actionLink(ns(deleteBtnId), "Remove"), style = "padding-top: 7px"),
      column(2, textInput(ns(groupNameId),
                          label = NULL,
                          placeholder = "Type group name")),
      # Selection input
      column(5, selectizeInput(ns(regionSelectId),
                               label = NULL,
                               choices = distr$FullRegionOfOrigin,
                               multiple = TRUE,
                               options = list(placeholder = "Select regions in this group"))),
      column(4, textOutput(ns(distCountId),
                           inline = TRUE))
    )

    # # EVENT: Adjustment selection changed
    # observeEvent(input[[adjustSelectId]], {
    #   # Add selected adjustment to the list of adjustments to run
    #   selectedAdjustmentFileName <- input[[adjustSelectId]]
    #   if (file.exists(selectedAdjustmentFileName)) {
    #     # Get adjustment specification and enrich with key for later reference
    #     adjustmentSpec <- GetListObject(selectedAdjustmentFileName)
    #     adjustmentSpec$Key <- key
    #     vals$adjustmentSpecs[[key]] <- adjustmentSpec
    #   }
    # })

    # EVENT: Button "Remove" clicked
    observeEvent(input[[deleteBtnId]], {
      removeUI(selector = paste0("#", ns(rowId)))
    })

    # EVENT: Group name edited
    observeEvent(input[[groupNameId]], {
      groupName <- input[[groupNameId]]
    })

    # EVENT: Regions selected
    observeEvent(input[[regionSelectId]], {
      groupRegions <- input[[regionSelectId]]
    })

    # Store for next adjustment selection widget addition
    vals$lastGroupWidgetIndex <- index

    return(widget)
  }

  distr <- reactive({
    inputData <- req(inputData()$Table)
    GetOriginDistribution(inputData)
  })

  map <- reactive({
    type <- req(input$groupSelect)
    distr <- req(distr())
    GetOriginGroupingMap(type, distr)
  })

  output[["groupingTableDiv"]] <- renderUI({
    type <- req(input$groupSelect)
    if (type %in% groupingNames[1:3]) {
      tableOutput(ns("groupingTableDT"))
    } else {
      uiOutput(ns("groupingTableUI"))
    }
  })

  output[["regionOfOriginTable"]] <- renderTable({
    req(distr())
  })

  output[["groupingTableDT"]] <- renderTable({
    type <- req(input$groupSelect)
    req(type %in% groupingNames[1:3])
    distr <- req(distr())
    map <- req(map())
    GetGroupingTable(type, distr, map)
  }, width = "100%")

  output[["groupingTableUI"]] <- renderUI({
    distr <- req(distr())
    tagList(
      # tags$table(
      #   class = "table shiny-table table- spacing-s",
      #   style = "width: 100%",
      #   tags$thead(
      #     tags$tr(
      #       tags$th(""),
      #       tags$th("GroupedRegionOfOrigin"),
      #       tags$th("FullRegionOfOrigin"),
      #       tags$th("Count")
      #     )
      #   ),
      #   tags$tbody(
      #     tags$tr(
      #       tags$td(actionLink(ns("deleteGroupBtn"), "Remove")),
      #       tags$td(textInput(ns("text"),
      #                         label = NULL)),
      #       tags$td(selectInput(ns("select"),
      #                           label = NULL,
      #                           choices = distr$FullRegionOfOrigin,
      #                           multiple = TRUE)),
      #       tags$td(10)
      #     )
      #   )
      # ),
      div(id = ns("groupsList")),
      actionButton(ns("addGroupBtn"), "Add")
    )
  })

  # Add adjustment selection widget
  observeEvent(input[["addGroupBtn"]], {
    widget <- GetGroupCreateWidget()
    insertUI(selector = paste0("#", ns("groupsList")),
             where = "beforeEnd",
             ui = widget)
  })

  inputDataWithMapping <- reactive({
    inputData <- req(inputData())
    map <- req(map())
    ApplyOriginGroupingMap(inputData, map)
  })

  return(inputDataWithMapping)
}
