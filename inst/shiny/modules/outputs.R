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
      uiOutput(ns("downloadLinks"))
    ),
    uiOutput(ns("adjustedDataTableBox"))
  )
}

# Server logic
outputs <- function(input, output, session, adjustedData)
{
  # Get namespace
  ns <- session$ns

  finalData <- reactiveVal(NULL)
  rdDistribution <- reactiveVal(NULL)

  observeEvent(adjustedData(), {
    adjustedData <- adjustedData()
    isolate({
      finalData(adjustedData[[length(adjustedData)]])

      rdAdjIdx <- which(sapply(adjustedData, "[[", "Type") == "REPORTING_DELAYS")
      if (length(rdAdjIdx) > 0 && rdAdjIdx > 0) {
        rdDistribution(adjustedData[[rdAdjIdx]]$Artifacts$RdDistribution)
      }
    })
  })

  # EVENT: Button "Download csv/stata data" clicked
  DownloadIntermediateData <- function(type = "ADJUSTED_DATA", format) {

    timeStamp <- finalData()$TimeStamp

    if (type == "ADJUSTED_DATA") {
      downloadData <- finalData()
      fileNamePrefix <- "AdjustedData"
    } else if (type == "RD_DISTRIBUTION") {
      downloadData <- rdDistribution()
      fileNamePrefix <- "RdDistribution"
    } else {
      downloadData <- NULL
    }

    if (!is.null(downloadData) &&
        type == "ADJUSTED_DATA" &&
        format %in% c("csv", "dta")) {
      downloadData <- downloadData$Table
    }

    downloadHandler(
      filename = function() {
        fileName <- sprintf("%s_%s.%s",
                            fileNamePrefix,
                            timeStamp,
                            format)
        return(fileName)
      },
      content = function(file) {
        withProgress(message = "Creating download data output file",
                     detail = "The file will be available for download shortly.",
                     value = 0, {
                       setProgress(0)
                       WriteDataFile(downloadData, file)
                       setProgress(1)
                     })
        return(NULL)
      }
    )
  }

  output[["downloadLinks"]] <- renderUI({
    finalData <- finalData()
    rdDistribution <- rdDistribution()

    isolate({
      widget <- list()
      if (!is.null(finalData)) {

        widget[["AD_Header"]] <- h3("Adjusted data downloads")
        widget[["AD_Description"]] <- p("Adjusted data can be downloaded as:")
        widget[["AD_Buttons"]] <- tags$ul(
          tags$li(downloadLink(ns("downloadDataRdsBtn"), "R rds file")),
          tags$li(downloadLink(ns("downloadDataCsvBtn"), "csv file")),
          tags$li(downloadLink(ns("downloadDataStataBtn"), "Stata dta file"))
        )

        widget[["AD_HelpText"]] <- tagList(
          p("Data saved as R file (extension \"rds\") are lists with the following attributes:"),
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

        output[["downloadDataRdsBtn"]] <- DownloadIntermediateData(type = "ADJUSTED_DATA", "rds")
        output[["downloadDataCsvBtn"]] <- DownloadIntermediateData(type = "ADJUSTED_DATA", "csv")
        output[["downloadDataStataBtn"]] <- DownloadIntermediateData(type = "ADJUSTED_DATA", "dta")

        if (!is.null(rdDistribution)) {
          widget[["RD_Header"]] <- h3("Reporting delays downloads")
          widget[["RD_Description"]] <- p("Reporting delays distributions can be downloaded as:")
          widget[["RD_Buttons"]] <- tags$ul(
            tags$li(downloadLink(ns("downloadDistrRdsBtn"), "R rds file")),
            tags$li(downloadLink(ns("downloadDistrCsvBtn"), "csv file")),
            tags$li(downloadLink(ns("downloadDistrStataBtn"), "Stata dta file"))
          )

          output[["downloadDistrRdsBtn"]] <- DownloadIntermediateData(type = "RD_DISTRIBUTION", "rds")
          output[["downloadDistrCsvBtn"]] <- DownloadIntermediateData(type = "RD_DISTRIBUTION", "csv")
          output[["downloadDistrStataBtn"]] <- DownloadIntermediateData(type = "RD_DISTRIBUTION", "dta")
        }

      } else {
        widget <- p("Please, run adjustments first.")
      }
    })

    return(tagList(widget))
  })

  output[["adjustedDataTableBox"]] <- renderUI({
    finalData <- finalData()$Table

    isolate({
      if (!is.null(finalData)) {
        box(
          width = 12,
          title = "Adjusted data records",
          solidHeader = FALSE,
          status = "warning",
          collapsible = TRUE,
          dataTableOutput(ns("adjustedDataTable"))
        )
      }
    })
  })

  output[["adjustedDataTable"]] <- renderDataTable(finalData()$Table,
                                                   options = list(
                                                     dom = '<"top">lirt<"bottom">p',
                                                     autoWidth = FALSE,
                                                     pageLength = 25,
                                                     scrollX = TRUE,
                                                     deferRender = TRUE,
                                                     serverSide = TRUE,
                                                     scroller = FALSE))
}
