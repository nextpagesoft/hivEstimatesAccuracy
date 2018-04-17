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
      uiOutput(ns("downloadLinks")))
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
      intermediateData <- adjustedData[[key]]$Table
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
      downloadCsvBtnId  <- paste0("downloadCsvBtn", runIdx)
      downloadStataBtnId  <- paste0("downloadStataBtn", runIdx)

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
          downloadCsvBtnId  <- paste0("downloadCsvBtn", runIdx)
          downloadStataBtnId  <- paste0("downloadStataBtn", runIdx)

          widget[[as.character(runIdx)]] <- fluidRow(
            id = ns(runIdx),
            column(3, sprintf("%d. %s", runIdx, name)),
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
