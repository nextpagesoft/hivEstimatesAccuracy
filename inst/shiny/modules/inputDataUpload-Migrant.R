groupingNames <-
  c("REPCOUNTRY + UNK + OTHER",
    "REPCOUNTRY + UNK + SUBAFR + OTHER",
    "REPCOUNTRY + UNK + 4 most prevalent other regions")

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
            uiOutput(ns("migrant4TableDiv"))
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

  # inputDataWithMapping <- reactiveVal(NULL)

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
    tags$table(
      tags$thead(
        tags$tr(
          tags$th("Group"),
          tags$th("RegionOfOrigin"),
          tags$th("Count")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("UNK"),
          tags$td("UNK"),
          tags$td(10)
        )
      )
    )
  })

  inputDataWithMapping <- reactive({
    inputData <- req(inputData())
    map <- req(map())
    ApplyOriginGroupingMap(inputData, map)
  })

  return(inputDataWithMapping)
}
