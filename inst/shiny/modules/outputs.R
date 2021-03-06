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
      h3("Application state data downloads"),
      tags$ul(
        tags$li(downloadLink(ns("downloadStateRdsBtn"), "R rds file"))
      ),
      p("This file contains the full state of the application. Loading it later will restore the application to the current state."),
      uiOutput(ns("hivDownloadLinks")),
      uiOutput(ns("adjDownloadLinks"))
    ),
    uiOutput(ns("adjustedDataTableBox"))
  )
}

# Server logic
outputs <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  finalData <- reactiveVal(NULL)
  rdDistribution <- reactiveVal(NULL)
  hivModelData <- reactiveVal(NULL)

  # EVENT: Button "Download csv/stata data" clicked
  DownloadData <- function(type, format) {
    if (type == "ADJUSTED_DATA") {
      downloadData <- finalData()
      fileNamePrefix <- "AdjustedData"
      timeStamp <- finalData()$TimeStamp
    } else if (type == "RD_DISTRIBUTION") {
      downloadData <- rdDistribution()
      fileNamePrefix <- "RdDistribution"
      timeStamp <- finalData()$TimeStamp
    } else if (type == "HIV_MODEL_DATA") {
      downloadData <- hivModelData()
      fileNamePrefix <- "HIVModelData"
      timeStamp <- GetTimeStamp()
    } else {
      downloadData <- NULL
    }

    if (!is.null(downloadData) && type == "ADJUSTED_DATA" && format %in% c("csv", "dta")) {
      downloadData <- downloadData$Table
    }

    downloadHandler(
      filename = function() {
        fileName <- sprintf("%s_%s.%s", fileNamePrefix, timeStamp, format)
        print(fileName)
        return(fileName)
      },
      content = function(file) {
        print(file)
        print(downloadData)
        withProgress(
          message = "Creating download data output file",
          detail = "The file will be available for download shortly.",
          value = 0, {
            setProgress(0)
            WriteDataFile(downloadData, file)
            setProgress(1)
          }
        )
        return(NULL)
      }
    )
  }

  observe({
    adjustedData <- appStatus$AdjustedData
    isolate({
      if (!is.null(adjustedData)) {
        finalData(adjustedData[[length(adjustedData)]])
        rdAdjIdx <- which(sapply(adjustedData, "[[", "Type") == "REPORTING_DELAYS")
        if (length(rdAdjIdx) > 0 && rdAdjIdx > 0) {
          rdDistribution(adjustedData[[rdAdjIdx]]$Artifacts$RdDistribution)
        }
      } else {
        finalData(NULL)
        rdDistribution(NULL)
      }
    })
  })

  observe({
    hivModelData <- appStatus$HIVModelData
    isolate({
      if (!is.null(hivModelData)) {
        hivModelData(hivModelData)
      } else {
        hivModelData(NULL)
      }
    })
  })

  output[['downloadStateRdsBtn']] <- downloadHandler(
    filename = function() {
      return(sprintf("HIV_state_%s.rds", GetTimeStamp()))
    },
    content = function(file) {
      withProgress(
        message = "Creating state file",
        detail = "The file will be available for download shortly.",
        value = 0, {
          setProgress(0)
          WriteDataFile(reactiveValuesToList(appStatus), file)
          setProgress(1)
        }
      )
      return(NULL)
    }
  )

  output[["hivDownloadLinks"]] <- renderUI({
    hivModelData <- hivModelData()
    widget <- list()

    isolate({
      if (!is.null(hivModelData)) {
        widget[["HD_Header"]] <- h3("HIV Modelling data downloads")
        widget[["HD_Description"]] <- p("Input data can be downloaded as:")
        widget[["HD_Buttons"]] <- tags$ul(
          tags$li(downloadLink(ns("hivDownloadDataRdsBtn"), "R rds file")),
          tags$li(downloadLink(ns("hivDownloadDataZipBtn"), "Zipped HIV Modelling input data files"))
        )

        output[["hivDownloadDataRdsBtn"]] <- DownloadData(type = "HIV_MODEL_DATA", "rds")
        output[["hivDownloadDataZipBtn"]] <- DownloadData(type = "HIV_MODEL_DATA", "zip")
      } else {
        widget <- p("Download links for HIV Modelling input data will be available after uploading TESSy format data.")
      }
    })

    return(tagList(widget))
  })


  output[["adjDownloadLinks"]] <- renderUI({
    finalData <- finalData()
    rdDistribution <- rdDistribution()

    widget <- list()

    isolate({
      if (!is.null(finalData)) {
        widget[["AD_Header"]] <- h3("Adjusted data downloads")
        widget[["AD_Description"]] <- p("Adjusted data can be downloaded as:")
        widget[["AD_Buttons"]] <- tags$ul(
          tags$li(downloadLink(ns("adjDownloadDataRdsBtn"), "R rds file")),
          tags$li(downloadLink(ns("adjDownloadDataCsvBtn"), "csv file")),
          tags$li(downloadLink(ns("adjDownloadDataStataBtn"), "Stata dta file"))
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

        output[["adjDownloadDataRdsBtn"]] <- DownloadData(type = "ADJUSTED_DATA", "rds")
        output[["adjDownloadDataCsvBtn"]] <- DownloadData(type = "ADJUSTED_DATA", "csv")
        output[["adjDownloadDataStataBtn"]] <- DownloadData(type = "ADJUSTED_DATA", "dta")

        if (!is.null(rdDistribution)) {
          widget[["RD_Header"]] <- h3("Reporting delays downloads")
          widget[["RD_Description"]] <- p("Reporting delays distributions can be downloaded as:")
          widget[["RD_Buttons"]] <- tags$ul(
            tags$li(downloadLink(ns("adjDownloadDistrRdsBtn"), "R rds file")),
            tags$li(downloadLink(ns("adjDownloadDistrCsvBtn"), "csv file")),
            tags$li(downloadLink(ns("adjDownloadDistrStataBtn"), "Stata dta file"))
          )

          output[["adjDownloadDistrRdsBtn"]] <- DownloadData(type = "RD_DISTRIBUTION", "rds")
          output[["adjDownloadDistrCsvBtn"]] <- DownloadData(type = "RD_DISTRIBUTION", "csv")
          output[["adjDownloadDistrStataBtn"]] <- DownloadData(type = "RD_DISTRIBUTION", "dta")
        }
      } else {
        widget <- p("Download links for adjusted data will be available after performing adjustments.")
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

  output[["adjustedDataTable"]] <- renderDataTable(
    finalData()$Table,
    options = list(
      dom = '<"top">lirt<"bottom">p',
      autoWidth = FALSE,
      pageLength = 25,
      scrollX = TRUE,
      deferRender = TRUE,
      serverSide = TRUE,
      scroller = FALSE
    )
  )
}
