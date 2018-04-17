# User interface
inputDataUploadUI <- function(id)
{
  ns <- NS(id)

  tagList(
    div(style = "margin-top: 15px"),
    box(
      width = 12,
      title = "Input data",
      solidHeader = FALSE,
      status = "primary",
      collapsible = TRUE,
      fluidRow(
        column(width = 4,
               fileInput(ns("fileInput"),
                         width = "100%",
                         label = "File input:"),
               p("Maximum file size: 70MB",
                 tags$br(),
                 "Supported files types: txt, csv, xls, xlsx (uncompressed and zip archives)")),
        column(width = 8,
               withSpinner(uiOutput(ns("origDataDetails")),
                           type = 7,
                           proxy.height = "50px")))),
    shinyjs::hidden(
      div(
        id = ns("attrMappingBox"),
        box(
          width = 12,
          title = "Attributes mapping",
          solidHeader = FALSE,
          status = "warning",
          collapsible = TRUE,
          p(
            HTML(
              paste("Please, provide mapping between attributes used internally by the tool (column \"Attribute\") and the input data dimensions (column \"Input data column\").",
                    "If \"Input data column\" is not specifid, then value in column \"Default value\" is used.",
                    sep = "<br />"
              )
            )
          ),
          actionButton(ns("applyMappingBtn"),
                       label = "Apply mapping",
                       style = "margin-bottom: 15px"),
          fluidRow(
            column(8,
                   uiOutput(ns("attrMappingTableDiv"))),
            column(4,
                   uiOutput(ns("attrMappingStatusBox")),
                   uiOutput(ns("valueCheckStatusBox"))
            )
          )
        )
      )
    )
  )
}

# Server logic
inputDataUpload <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  # Data related reactive values
  originalData <- reactiveVal(NULL)
  inputData <- reactiveVal(NULL)
  inputDataTest <- reactiveVal(NULL)

  # Local state reactive values
  vals <- reactiveValues(AttrMapping = list(),
                         DefaultValues = list(),
                         OriginalDataAttrs = c(),
                         AttrMappingStatus = NULL,
                         InputDataTestStatus = NULL)

  # EVENT: Input data file name changed
  originalData <- eventReactive(input[["fileInput"]], {
    # Get reference to file input
    fileInput <- input$fileInput

    # Validate
    validate(need(fileInput, message = FALSE))

    # Read input data
    originalData <- ReadDataFile(fileInput$datapath)
    originalDataAttrs <- colnames(originalData)

    # Initialize the local state
    vals$DefaultValues     <- GetPreliminaryDefaultValues()
    vals$AttrMapping       <- GetPreliminaryAttributesMapping(originalData)
    vals$OriginalDataAttrs <- originalDataAttrs

    # Apply a specific column mapping for column "FirstCD4Count" column
    if ("FirstCD4Count" %in% names(vals$AttrMapping) &
        "cd4_num" %in% tolower(originalDataAttrs)) {
      vals$AttrMapping[["FirstCD4Count"]] <-
        originalDataAttrs[tolower(originalDataAttrs) == "cd4_num"][1]
    }

    appStatus$InputDataUploaded <- TRUE
    appStatus$AttributeMappingValid <- FALSE

    return(originalData)
  }, ignoreNULL = TRUE)

  # Show attributes mapping section
  observe({
    originalData <- originalData()
    if (is.null(originalData)) {
      shinyjs::hide("attrMappingBox")
    } else {
      shinyjs::show("attrMappingBox")
    }
  })

  # Output original data details when they have changed
  output[["origDataDetails"]] <- renderUI({
    # Respond to originalData change
    originalData <- originalData()

    isolate({
      # Get reference to file input
      fileInput <- input$fileInput

      # Get input data details
      origDataDetails <-
        fluidRow(
          column(3,
                 p(strong("Name:"), fileInput$name),
                 p(strong("Size:"), FormatObjectSize(fileInput$size)),
                 p(strong("Type:"), fileInput$type),
                 p(strong("Number of records:"), nrow(originalData))),
          column(9,
                 p(strong("Column names:")),
                 p(id = ns("colNamesP"),
                   paste(colnames(originalData), collapse = ", ")))
        )

      return(origDataDetails)
    })
  })

  # Generate attributes mapping table
  output[["attrMappingTableDiv"]] <- renderUI({
    # Respond to originalData change
    originalData()

    isolate({

      ns <- session$ns

      # 1) Define rows
      attrMappingRows <- lapply(names(vals$AttrMapping), function(analysisAttr) {
        inputAttr <- vals$AttrMapping[[analysisAttr]]

        selectOptions <- list()
        if (is.null(inputAttr)) {
          selectOptions <- utils::modifyList(selectOptions,
                                             list(onInitialize = I('function() { this.setValue(""); }')))
        }

        selectId <- paste0("attr_", analysisAttr)
        defValInputId <- paste0("defVal_", analysisAttr)

        widget <- tags$tr(
          # Selection attribute
          tags$td(analysisAttr),
          # Selection data dimension
          tags$td(selectizeInput(ns(selectId),
                                 label = NULL,
                                 choices = vals$OriginalDataAttrs,
                                 selected = inputAttr,
                                 options = selectOptions)),
          tags$td(textInput(ns(defValInputId), NULL))
        )

        # EVENT: Selected item changed
        observeEvent(input[[selectId]], {
          mappedAttr <- input[[selectId]]
          vals$AttrMapping[[analysisAttr]] <- mappedAttr
          if (!IsEmptyString(mappedAttr)) {
            shinyjs::disable(defValInputId)
          } else {
            shinyjs::enable(defValInputId)
          }
          vals$AttrMappingStatus <- NULL
          vals$InputDataTestStatus <- NULL
        })

        observeEvent(input[[defValInputId]], {
          vals$DefaultValues[[analysisAttr]] <- input[[defValInputId]]
          vals$InputDataTestStatus <- NULL
        })

        return(widget)
      })

      # 2) Define table
      attrMappingTableDiv <- tags$table(
        id = ns("attrMappingTable"),
        tags$thead(
          tags$tr(
            tags$th("Attribute", class = ns("attrNameCol")),
            tags$th("Input data column"),
            tags$th("Default value")
          )
        ),
        tags$tbody(
          attrMappingRows
        )
      )

      return(attrMappingTableDiv)
    })
  })

  # Respond to "Apply mapping" button click
  observeEvent(input[["applyMappingBtn"]], {
    withProgress(message = "Attributes mapping",
                 value = 0, {
                   originalData <- originalData()
                   setProgress(0.1, detail = "Applying mapping")

                   inputDataTest <- ApplyAttributesMapping(originalData,
                                                           vals$AttrMapping,
                                                           vals$DefaultValues)
                   setProgress(0.4, detail = "Pre-processing data")

                   inputDataTest <- PreProcessInputData(inputDataTest)
                   setProgress(0.9, detail = "Checking data validity")

                   vals$AttrMappingStatus <- GetAttrMappingStatus(vals$AttrMapping)
                   vals$InputDataTestStatus <- GetInputDataValidityStatus(inputDataTest$Table)

                   setProgress(1, detail = "Done")
                 })

    inputDataTest(inputDataTest)
  })

  # Populate attribute mapping status UI
  output[["attrMappingStatusBox"]] <- renderUI({
    attrMappingStatus <- vals$AttrMappingStatus

    if (is.null(attrMappingStatus)) {
      return(NULL)
    }

    isolate({
      multipleMappedDataAttrs <- attrMappingStatus$MultipleMapped

      header <- NULL
      textColor <- NULL
      if (attrMappingStatus$Valid) {
        textColor <- "seagreen"
        headerHTML <- p(tags$i(class = "material-icons", "check"),
                        "Assignement of input data columns to attributes is valid.",
                        style = paste("color:", textColor))
      } else {
        textColor <- "red"
        headerHTML <- p(tags$i(class = "material-icons", "error_outline"),
                        "Assignement of input data columns to attributes is not valid.",
                        style = paste("color:", textColor))
      }

      multiMappedStatusHTML <- NULL
      if (length(multipleMappedDataAttrs) > 0) {
        multiMappedStatusHTML <- tagList(
          tags$ol(
            lapply(names(multipleMappedDataAttrs), function(dataAttr) {
              analysisAttrs <- multipleMappedDataAttrs[[dataAttr]]
              tags$li(sprintf("Input data column %s is mapped to attributes %s.",
                              AddQuoteMarks(dataAttr),
                              paste(AddQuoteMarks(analysisAttrs), collapse = ", ")))
            })
          )
        )
      }

      attrMappingStatusBox <- box(width = 12,
                                  style = "background-color: ghostwhite;",
                                  headerHTML,
                                  multiMappedStatusHTML)

      return(attrMappingStatusBox)
    })
  })

  output[["valueCheckStatusBox"]] <- renderUI({
    # Respond to InputDataTestStatus change
    inputDataTestStatus <- vals$InputDataTestStatus

    if (is.null(inputDataTestStatus)) {
      return(NULL)
    }

    isolate({
      wrongValuesCols <- Filter(function(x) length(x$WrongValues) != 0,
                                inputDataTestStatus$CheckStatus)

      headerHTML <- NULL
      textColor <- NULL
      if (inputDataTestStatus$Valid) {
        textColor <- "seagreen"
        headerHTML <- p(tags$i(class = "material-icons", "check"),
                        "Input data values are valid.",
                        style = paste("color:", textColor))
      } else {
        textColor <- "red"
        headerHTML <- p(tags$i(class = "material-icons", "error_outline"),
                        HTML("Input data values are not valid."),
                        style = paste("color:", textColor))
      }

      wrongValuesHTML <- NULL
      if (length(wrongValuesCols) > 0) {
        wrongValuesHTML <- tags$ol(
          lapply(names(wrongValuesCols), function(colName) {
            wrongValuesCol <- wrongValuesCols[[colName]]
            tags$li(sprintf("Attribute %s contains invalid value(s) %s.",
                            AddQuoteMarks(colName),
                            paste(AddQuoteMarks(wrongValuesCol$WrongValues), collapse = ", ")))
          })
        )
      }

      valueCheckStatusBox <- box(width = 12,
                                 style = "background-color: ghostwhite;",
                                 headerHTML,
                                 wrongValuesHTML)

      return(valueCheckStatusBox)
    })
  })

  observeEvent(c(vals$InputDataTestStatus$Valid,
                 vals$AttrMappingStatus$Valid), {
    if (is.null(vals$AttrMappingStatus) | is.null(vals$InputDataTestStatus)) {
      inputData(NULL)
      appStatus$AttributeMappingValid <- FALSE
    } else if (vals$AttrMappingStatus$Valid & vals$InputDataTestStatus$Valid) {
      inputData(inputDataTest())
      appStatus$AttributeMappingValid <- TRUE
    } else {
      inputData(NULL)
      appStatus$AttributeMappingValid <- FALSE
    }
  })

  return(inputData)
}
