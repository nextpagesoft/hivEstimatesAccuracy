# Allow uploading files up to 70MB in size
options(shiny.maxRequestSize = 70 * 1024^2)

# Determine if the app is run on the server or locally
isServer <- Sys.info()[["nodename"]] == "ShinyServer"

# Server specific code
if (isServer) {
  .libPaths(c("/home/daniel/R/devel/hivEstimatesAccuracy/packrat/lib/x86_64-pc-linux-gnu/3.4.3"))
}

# Load standard libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)

# Load main library
library(hivEstimatesAccuracy)

# Load application modules
modulesPath <- system.file("shiny/modules", package = "hivEstimatesAccuracy")
source(file.path(modulesPath, "introduction.R"))
source(file.path(modulesPath, "inputDataUpload.R"))
source(file.path(modulesPath, "dataSummary.R"))
source(file.path(modulesPath, "dataAdjust.R"))
source(file.path(modulesPath, "createReports.R"))
source(file.path(modulesPath, "settings.R"))
source(file.path(modulesPath, "outputs.R"))

# App globals
titleString <- paste0("HIV Estimates Accuracy v.", packageVersion("hivEstimatesAccuracy"))

# Define application user interface
ui <- tagList(
  shinyjs::useShinyjs(),

  dashboardPage(
    dashboardHeader(title = titleString,
                    titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction",       tabName = "intro",       icon = icon("home")),
        menuItem("Input data upload",  tabName = "upload",      icon = icon("upload")),
        menuItem("Input data summary", tabName = "summary",     icon = icon("bar-chart")),
        menuItem("Adjustments",        tabName = "adjustments", icon = icon("bolt")),
        menuItem("Reports",            tabName = "reports",     icon = icon("book")),
        menuItem("Outputs",            tabName = "outputs",     icon = icon("download"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "./css/style.css")
      ),
      tabItems(
        tabItem(tabName = "intro",       fluidRow(introductionUI("intro"))),
        tabItem(tabName = "upload",      fluidRow(inputDataUploadUI("upload"))),
        tabItem(tabName = "summary",     fluidRow(dataSummaryUI("summary"))),
        tabItem(tabName = "adjustments", fluidRow(dataAdjustUI("adjustments"))),
        tabItem(tabName = "reports",     fluidRow(createReportsUI("reports"))),
        tabItem(tabName = "outputs",     fluidRow(outputsUI("outputs")))
      )
    )
  )
)

# Define application server logic
server <- function(input, output, session)
{
  appStatus <- reactiveValues(InputDataUploaded = FALSE,
                              AttributeMappingValid = FALSE)

  callModule(introduction, "intro")
  inputData <- callModule(inputDataUpload, "upload", appStatus)
  callModule(dataSummary, "summary", appStatus, inputData)
  adjustedData <- callModule(dataAdjust, "adjustments", inputData)
  callModule(createReports, "reports", adjustedData)
  callModule(outputs, "outputs", adjustedData)

  if (!isServer) {
    session$onSessionEnded(stopApp)
  }
}

# Run application
shinyApp(ui, server,
         options = c(display.mode = "normal",
                     test.mode = FALSE))
