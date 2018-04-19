# Module globals
# NONE

# User interface
outputsUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "Outputs",
      solidHeader = FALSE,
      collapsible = TRUE,
      status = "primary",
      uiOutput(ns("downloadLinks")),
      br(),
      p("Data saved as R file (extension 'rds') are lists with the following attributes:"),
      tags$ol(
        tags$li("Table: data.frame with the adjusted data"),
        tags$li("Artifacts: list with artifact objects (data.frames, plots) produced during the adjustment"),
        tags$li("Parameters: list with the parameters applied"),
        tags$li("RunIdx: Order of the adjustment"),
        tags$li("Name: Name of the adjustment"),
        tags$li("Type: Type of the adjustment"),
        tags$li("SubType: Sub-type of the adjustment"),
        tags$li("TimeStamp: Adjustment execution end time in format 'YYYYMMDDhhmmss'")
      ),
      p("Data saved as csv (extension 'csv') or Stata (extension 'dta') file contain only the adjusted data.")
    )
  )
}

# Server logic
outputs <- function(input, output, session, adjustedData)
{
  # Get namespace
  ns <- session$ns

  # EVENT: Button "Download csv/stata data" clicked
  DownloadIntermediateData <- function(key, format) {
    adjustedData <- adjustedData()
    if (key %in% names(adjustedData)) {
      if (format %in% c("csv", "dta")) {
        intermediateData <- adjustedData[[key]]$Table
      } else if (format %in% c("rds")) {
        intermediateData <- adjustedData[[key]]
      } else {
        intermediateData <- NULL
      }
    } else {
      intermediateData <- NULL
    }

    downloadHandler(
      filename = function() {
        fileName <- sprintf("%d. %s_%s.%s",
                            adjustedData[[key]]$RunIdx,
                            adjustedData[[key]]$Name,
                            adjustedData[[key]]$TimeStamp,
                            format)
        return(fileName)
      },
      content = function(file) {
        withProgress(message = "Creating intermediate data output file",
                     detail = "The file will be available for download shortly.",
                     value = 0, {
                       setProgress(0)
                       WriteDataFile(intermediateData, file)
                       setProgress(1)
                     })
        return(NULL)
      }
    )
  }

  EnableIntermediateDataDownloadLinks <- function() {

    adjustedData <- adjustedData()

    runIndices <- sort(sapply(adjustedData, "[[", "RunIdx"))
    for (runIdx in runIndices) {
      key <- names(runIndices)[runIdx]
      downloadRdsBtnId  <- paste0("downloadRdsBtn", runIdx)
      downloadCsvBtnId  <- paste0("downloadCsvBtn", runIdx)
      downloadStataBtnId  <- paste0("downloadStataBtn", runIdx)

      output[[downloadRdsBtnId]] <- DownloadIntermediateData(key, "rds")
      output[[downloadCsvBtnId]] <- DownloadIntermediateData(key, "csv")
      output[[downloadStataBtnId]] <- DownloadIntermediateData(key, "dta")
    }
  }

  output[["downloadLinks"]] <- renderUI({
    adjustedData <- adjustedData()

    widget <- list()

    isolate({
      if (!is.null(adjustedData)) {

        widget[["Header"]] <- p("Download links below provide access to data after each adjustment.")

        runIndices <- sort(sapply(adjustedData, "[[", "RunIdx"))
        for (runIdx in runIndices) {
          key <- names(runIndices)[runIdx]
          name <- adjustedData[[key]]$Name
          downloadRdsBtnId  <- paste0("downloadRdsBtn", runIdx)
          downloadCsvBtnId  <- paste0("downloadCsvBtn", runIdx)
          downloadStataBtnId  <- paste0("downloadStataBtn", runIdx)

          widget[[as.character(runIdx)]] <- fluidRow(
            id = ns(runIdx),
            column(3, sprintf("%d. %s", runIdx, name)),
            column(2, downloadLink(ns(downloadRdsBtnId), "Download as R file")),
            column(2, downloadLink(ns(downloadCsvBtnId), "Download as csv file")),
            column(2, downloadLink(ns(downloadStataBtnId), "Download as Stata file"))
          )
        }

        EnableIntermediateDataDownloadLinks()
      } else {
        widget <- p("Please, run adjustments first.")
      }
    })

    return(tagList(widget))
  })

}
