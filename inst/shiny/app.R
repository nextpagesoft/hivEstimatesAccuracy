isLocalRun <- TRUE

users <- reactiveValues(count = 0)

# Allow uploading files up to 70MB in size
options(shiny.maxRequestSize = 70 * 1024^2)

# Load standard libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinycssloaders))

# Load main library
library(hivEstimatesAccuracy)

# Load application modules
modulesPath <- system.file("shiny/modules", package = "hivEstimatesAccuracy")
wwwPath <- system.file("shiny/www", package = "hivEstimatesAccuracy")
source(file.path(modulesPath, "welcome.R"))
source(file.path(modulesPath, "inputDataUpload.R"))
source(file.path(modulesPath, "dataSummary.R"))
source(file.path(modulesPath, "dataAdjust.R"))
source(file.path(modulesPath, "createReports.R"))
source(file.path(modulesPath, "outputs.R"))
source(file.path(modulesPath, "manual.R"))
source(file.path(modulesPath, "hivModel.R"))

addResourcePath("www", wwwPath)

# App globals
titleString <- "HIV Tools"
version <- as.character(packageDescription(pkg = "hivEstimatesAccuracy", fields = "Version"))

# Define application user interface
ui <- tagList(
  shinyjs::useShinyjs(),

  dashboardPage(
    tags$header(
      class = "main-header",
      span(class = "logo", titleString),
      tags$nav(
        class = "navbar navbar-static-top",
        div(class = "navbar-custom-menu",
            div(sprintf("v. %s", version)),
            textOutput("userCount"),
            div(tags$a(
              href = "./",
              target = "_blank",
              list(icon("external-link"), "Open new instance in separate tab")
            )),
            actionLink("setSeed", "Set seed", icon = icon("random"))
        )
      )
    ),
    dashboardSidebar(
      disable = TRUE,
      sidebarMenuOutput("menu"),
      width = 180
    ),
    dashboardBody(
      tags$head(
        tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=UA-125099925-2"),
        includeScript(path = file.path(wwwPath, "/js/google_analytics.js")),
        tags$link(rel = "stylesheet", type = "text/css", href = "./www/css/style.css"),
        tags$title(titleString)
      ),
      tabItems(
        tabItem(tabName = "welcome",     fluidRow(welcomeUI("welcome"))),
        tabItem(tabName = "upload",      fluidRow(inputDataUploadUI("upload"))),
        tabItem(tabName = "summary",     fluidRow(dataSummaryUI("summary"))),
        tabItem(tabName = "adjustments", fluidRow(dataAdjustUI("adjustments"))),
        tabItem(tabName = "hivModel",    fluidRow(hivModelUI("hivModel"))),
        tabItem(tabName = "reports",     fluidRow(createReportsUI("reports"))),
        tabItem(tabName = "outputs",     fluidRow(outputsUI("outputs"))),
        tabItem(tabName = "manual",      fluidRow(manualUI("manual")))
      )
    )
  )
)

# Define application server logic
server <- function(input, output, session)
{
  appStatus <- reactiveValues(
    Mode = "WELCOME",
    CreateTime = Sys.time(),
    Version = version,
    Seed = NULL,
    FileName = "",
    StateUploading = FALSE,
    InputDataUploaded = FALSE,
    OriginalData = NULL,
    DefaultValues = list(),
    OriginalDataAttrs = c(),
    AttrMapping = list(),
    AttrMappingStatus = NULL,
    AttrMappingValid = FALSE,
    InputDataTest = NULL,
    InputDataTestStatus = NULL,
    DiagYearRange = NULL,
    DiagYearRangeApply = FALSE,
    NotifQuarterRange = NULL,
    NotifQuarterRangeApply = NULL,
    InputData = NULL,
    AdjustedData = NULL,
    HIVModelData = NULL,
    AdjustmentSpecs = adjustmentSpecs,
    MIAdjustmentName = "None",
    RDAdjustmentName = "None",
    RunLog = "",
    IntermReport = "",
    Report = ""
  )

  # shinyjs::removeClass(selector = "body", class = "sidebar-collapse")

  callModule(welcome,         "welcome",     appStatus)
  callModule(inputDataUpload, "upload",      appStatus)
  callModule(dataSummary,     "summary",     appStatus)
  callModule(dataAdjust,      "adjustments", appStatus)
  callModule(createReports,   "reports",     appStatus)
  callModule(outputs,         "outputs",     appStatus)
  callModule(hivModel,        "hivModel",    appStatus)
  callModule(manual,          "manual")

  observeEvent(input[["setSeed"]], {
    showModal(
      modalDialog(
        title = "Set seed",
        textInput("seed", label = "Seed value", value = appStatus$Seed),
        p("Give empty value or type 'default' to remove fixed seed"),
        footer = tagList(
          actionButton("seedDlgOk", "OK",
                       style = "background-color: #69b023; color: white"),
          modalButton("Cancel")
        ),
        size = "s"
      )
    )
  })

  observeEvent(input[["seedDlgOk"]], {
    seed <- input$seed
    if (seed == "" || tolower(seed) == "default") {
      appStatus$Seed <- NULL
    } else {
      appStatus$Seed <- seed
    }
    removeModal()
  })


  onSessionStart <- isolate({
    users$count <- users$count + 1
  })

  onSessionEnded(function() {
    isolate({
      users$count <- users$count - 1
      if (isLocalRun && users$count == 0) {
        stopApp()
      }
    })
  })

  output[["userCount"]] <- renderText({
    sprintf("Number of open instances: %d", users$count)
  })

  output[["menu"]] <- renderMenu({
    if (appStatus$Mode == "WELCOME") {
      sidebarMenu(
        id = "tabs",
        menuItem("Welcome",                tabName = "welcome",     icon = icon("door-open"))
      )
    } else if (appStatus$Mode == "MODELLING") {
      sidebarMenu(
        id = "tabs",
        menuItem("Home",                   tabName = "welcome",     icon = icon("door-open")),
        menuItem("Case based data upload", tabName = "upload",      icon = icon("upload")),
        menuItem("Data summary",           tabName = "summary",     icon = icon("bar-chart")),
        menuItem("HIV Modelling",          tabName = "hivModel",    icon = icon("calculator")),
        menuItem("Reports",                tabName = "reports",     icon = icon("book")),
        menuItem("Outputs",                tabName = "outputs",     icon = icon("download")),
        menuItem("Manual",                 tabName = "manual",      icon = icon("book"))
      )
    } else if (appStatus$Mode == "ACCURACY") {
      sidebarMenu(
        id = "tabs",
        menuItem("Home",                   tabName = "welcome",     icon = icon("door-open")),
        menuItem("Case based data upload", tabName = "upload",      icon = icon("upload")),
        menuItem("Data summary",           tabName = "summary",     icon = icon("bar-chart")),
        menuItem("HIV Estimates Accuracy", tabName = "adjustments", icon = icon("bolt")),
        menuItem("Reports",                tabName = "reports",     icon = icon("book")),
        menuItem("Outputs",                tabName = "outputs",     icon = icon("download")),
        menuItem("Manual",                 tabName = "manual",      icon = icon("book"))
      )
    } else if (appStatus$Mode == "FULL") {
      sidebarMenu(
        id = "tabs",
        menuItem("Home",                   tabName = "welcome",     icon = icon("door-open")),
        menuItem("Case based data upload", tabName = "upload",      icon = icon("upload")),
        menuItem("Data summary",           tabName = "summary",     icon = icon("bar-chart")),
        menuItem("HIV Estimates Accuracy", tabName = "adjustments", icon = icon("bolt")),
        menuItem("HIV Modelling",          tabName = "hivModel",    icon = icon("calculator")),
        menuItem("Reports",                tabName = "reports",     icon = icon("book")),
        menuItem("Outputs",                tabName = "outputs",     icon = icon("download")),
        menuItem("Manual",                 tabName = "manual",      icon = icon("book"))
      )
    }
  })

  observeEvent(input[["tabs"]], {
    if (input$tabs == "welcome") {
      appStatus$Mode <- "WELCOME"
    }
  })

  observeEvent(appStatus[["Mode"]], {
    if (appStatus$Mode == "WELCOME") {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      updateTabItems(session, "tabs", "welcome")
    } else {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      updateTabItems(session, "tabs", "upload")
    }
  })

}

# Run application
shinyApp(ui, server, options = c(display.mode = "normal", test.mode = FALSE))
