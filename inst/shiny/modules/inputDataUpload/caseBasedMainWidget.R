# Load application modules
modulesPath <- system.file('shiny/modules/inputDataUpload', package = 'hivEstimatesAccuracy')
source(file.path(modulesPath, 'caseBasedMigrantWidget.R'))

# User interface
caseBasedMainWidgetUI <- function(id)
{
  ns <- NS(id)

  tagList(
    shinyjs::hidden(
      div(
        id = ns('attrMappingBox'),
        box(
          width = 12,
          title = 'Attributes mapping',
          solidHeader = FALSE,
          status = 'warning',
          collapsible = TRUE,
          p(
            HTML(
              paste('Please, provide mapping between attributes used internally by the tool (column \'Attribute\') and the input data dimensions (column \'Input data column\').',
                    'If \'Input data column\' is not specifid, then value in column \'Default value\' is used.',
                    sep = '<br />'
              )
            )
          ),
          actionButton(
            ns('applyMappingBtn'),
            label = 'Apply mapping',
            style = 'margin-bottom: 15px; background-color: #69b023; color: white'
          ),
          fluidRow(
            column(8, uiOutput(ns('attrMappingTableDiv'))),
            column(
              4,
              uiOutput(ns('attrMappingInfoDiv')),
              uiOutput(ns('attrMappingStatusBox')),
              uiOutput(ns('valueCheckStatusBox'))
            )
          )
        )
      )
    ),
    shinyjs::hidden(div(id = ns('migrantModule'), caseBasedMigrantWidgetUI(ns('migrant')))),
    uiOutput(ns('inputDataTableBox'))
  )

}

# Server logic
caseBasedMainWidget <- function(input, output, session, appStatus)
{
  # Get namespace
  ns <- session$ns

  # Show attributes mapping section
  observe({
    originalData <- appStatus$OriginalData
    if (is.null(originalData)) {
      shinyjs::hide('attrMappingBox', anim = TRUE, animType = 'fade')
      shinyjs::hide('migrantModule', anim = TRUE, animType = 'fade')
    } else {
      shinyjs::show('attrMappingBox')
      shinyjs::show('migrantModule')
    }
  })

  # Generate attributes mapping table
  output[['attrMappingTableDiv']] <- renderUI({
    # Respond to originalData change
    req(appStatus$OriginalData)

    isolate({

      ns <- session$ns

      # 1) Define rows
      attrMappingRows <- lapply(names(appStatus$AttrMapping), function(analysisAttr) {
        inputAttr <- appStatus$AttrMapping[[analysisAttr]]

        selectOptions <- list()
        if (is.null(inputAttr)) {
          selectOptions <- utils::modifyList(selectOptions,
                                             list(onInitialize = I('function() { this.setValue(""); }')))
        }

        selectId <- paste0('attr_', analysisAttr)
        defValInputId <- paste0('defVal_', analysisAttr)

        widget <- tags$tr(
          # Selection attribute
          tags$td(analysisAttr),
          # Selection data dimension
          tags$td(selectizeInput(ns(selectId),
                                 label = NULL,
                                 choices = appStatus$OriginalDataAttrs,
                                 selected = inputAttr,
                                 options = selectOptions)),
          tags$td(shinyjs::disabled(textInput(ns(defValInputId), NULL)))
        )

        # EVENT: Selected input column changed
        observeEvent(input[[selectId]], {
          oldMappedAttr <- appStatus$AttrMapping[[analysisAttr]]
          newMappedAttr <- input[[selectId]]
          if (!appStatus$StateUploading) {
            appStatus$AttrMapping[[analysisAttr]] <- newMappedAttr
            appStatus$AttrMappingStatus       <- NULL
            appStatus$InputDataTestStatus     <- NULL
            appStatus$InputDataTest           <- NULL
            appStatus$InputDataBeforeGrouping <- NULL
          }

          if (!IsEmptyString(newMappedAttr)) {
            shinyjs::disable(defValInputId)
          } else {
            shinyjs::enable(defValInputId)
          }
        })

        # EVENT: Default value changed
        observeEvent(input[[defValInputId]], {
          oldDefVal <- appStatus$DefaultValues[[analysisAttr]]
          newDefVal <- input[[defValInputId]]

          if (!appStatus$StateUploading) {
            appStatus$DefaultValues[[analysisAttr]] <- newDefVal
            appStatus$InputDataTestStatus     <- NULL
            appStatus$InputDataTest           <- NULL
            appStatus$InputDataBeforeGrouping <- NULL
          }
        })

        return(widget)
      })

      # 2) Define table
      attrMappingTableDiv <- tags$table(
        id = ns('attrMappingTable'),
        tags$thead(
          tags$tr(
            tags$th('Attribute', class = ns('attrNameCol')),
            tags$th('Input data column'),
            tags$th('Default value')
          )
        ),
        tags$tbody(
          attrMappingRows
        )
      )

      return(attrMappingTableDiv)
    })
  })

  output[['attrMappingInfoDiv']] <- renderUI({
    valid <- appStatus$AttrMappingValid

    isolate({
      text <- NULL
      if (valid) {
        text <- span(icon('check'),
                     'Applied mapping is valid. You can proceeed to \'Migrant variable regrouping\' section below or other tabs.',
                     style = 'color: seagreen')
      } else {
        text <- span('Input data has to be mapped to internal attributes and validated. Adjust mapping and press \'Apply mapping\' button to the left.')
      }

      attrMappingInfoBox <- box(width = 12,
                                style = 'background-color: ghostwhite;',
                                text)

      return(attrMappingInfoBox)
    })
  })

  # Respond to 'Apply mapping' button click
  observeEvent(input[['applyMappingBtn']], {
    withProgress(
      message = 'Attributes mapping',
      value = 0, {
        originalData <- appStatus$OriginalData
        setProgress(0.1, detail = 'Applying mapping')
        inputDataTest <- ApplyAttributesMapping(
          originalData, appStatus$AttrMapping, appStatus$DefaultValues
        )
        setProgress(0.4, detail = 'Pre-processing data with a single imputation of Gender')
        inputDataTest <- PreProcessInputDataBeforeSummary(inputDataTest, appStatus$Seed)
        setProgress(0.9, detail = 'Checking data validity')
        appStatus$AttrMappingStatus <- GetAttrMappingStatus(appStatus$AttrMapping)
        appStatus$InputDataTestStatus <- GetInputDataValidityStatus(inputDataTest$Table)
        appStatus$InputDataTest <- inputDataTest
        appStatus$InputUploading <- FALSE
        appStatus$StateUploading <- FALSE
        setProgress(1, detail = 'Done')
      }
    )
  })

  # Populate attribute mapping status UI
  output[['attrMappingStatusBox']] <- renderUI({
    attrMappingStatus <- req(appStatus$AttrMappingStatus)

    isolate({
      multipleMappedDataAttrs <- attrMappingStatus$MultipleMapped

      header <- NULL
      textColor <- NULL
      if (attrMappingStatus$Valid) {
        textColor <- 'seagreen'
        headerHTML <- span(icon('check'),
                           'Assignment of input data columns to attributes is valid.',
                           style = paste('color:', textColor))
      } else {
        textColor <- 'red'
        headerHTML <- span(icon('exclamation'),
                           'Assignment of input data columns to attributes is not valid.',
                           style = paste('color:', textColor))
      }

      multiMappedStatusHTML <- NULL
      if (length(multipleMappedDataAttrs) > 0) {
        multiMappedStatusHTML <- tagList(
          tags$ol(
            lapply(names(multipleMappedDataAttrs), function(dataAttr) {
              analysisAttrs <- multipleMappedDataAttrs[[dataAttr]]
              tags$li(sprintf('Input data column %s is mapped to attributes %s.',
                              AddQuoteMarks(dataAttr),
                              paste(AddQuoteMarks(analysisAttrs), collapse = ', ')))
            })
          )
        )
      }

      attrMappingStatusBox <- box(width = 12,
                                  style = 'background-color: ghostwhite;',
                                  headerHTML,
                                  multiMappedStatusHTML)

      return(attrMappingStatusBox)
    })
  })

  output[['valueCheckStatusBox']] <- renderUI({
    # Respond to InputDataTestStatus change
    inputDataTestStatus <- req(appStatus$InputDataTestStatus)

    isolate({
      wrongValuesCols <- Filter(function(x) length(x$WrongValues) != 0,
                                inputDataTestStatus$CheckStatus)

      headerHTML <- NULL
      textColor <- NULL
      if (inputDataTestStatus$Valid) {
        textColor <- 'seagreen'
        headerHTML <- span(icon('check'),
                           'Input data values are valid.',
                           style = paste('color:', textColor))
      } else {
        textColor <- 'red'
        headerHTML <- span(icon('exclamation'),
                           HTML('Input data values are not valid.'),
                           style = paste('color:', textColor))
      }

      wrongValuesHTML <- NULL
      if (length(wrongValuesCols) > 0) {
        wrongValuesHTML <- tags$ol(
          lapply(names(wrongValuesCols), function(colName) {
            wrongValuesCol <- wrongValuesCols[[colName]]
            tags$li(sprintf('Attribute %s contains invalid value(s) %s.',
                            AddQuoteMarks(colName),
                            paste(AddQuoteMarks(as.character(wrongValuesCol$WrongValues)), collapse = ', ')))
          })
        )
      }

      valueCheckStatusBox <- box(width = 12,
                                 style = 'background-color: ghostwhite;',
                                 headerHTML,
                                 wrongValuesHTML)

      return(valueCheckStatusBox)
    })
  })

  observeEvent(
    c(appStatus$AttrMappingStatus$Valid, appStatus$InputDataTestStatus$Valid),
    {
      if (is.null(appStatus$AttrMappingStatus) || is.null(appStatus$InputDataTestStatus)) {
        appStatus$InputDataBeforeGrouping <- NULL
        appStatus$AttrMappingValid <- FALSE
      } else if (appStatus$AttrMappingStatus$Valid && appStatus$InputDataTestStatus$Valid) {
        appStatus$InputDataBeforeGrouping <- appStatus$InputDataTest
        appStatus$AttrMappingValid <- TRUE
      } else {
        appStatus$InputDataBeforeGrouping <- NULL
        appStatus$AttrMappingValid <- FALSE
      }
    }
  )

  output[['inputDataTableBox']] <- renderUI({
    if (appStatus$AttrMappingValid) {
      box(
        width = 12,
        title = 'Input data records pre-processed',
        solidHeader = FALSE,
        status = 'warning',
        collapsible = TRUE,
        dataTableOutput(ns('inputDataTable'))
      )
    }
  })

  output[['inputDataTable']] <- renderDataTable({
    req(appStatus$InputData$Table)[, -c('GroupOfOrigin', 'SqCD4')]
  },
  options = list(
    dom = '<"top">lirt<"bottom">p',
    autoWidth = FALSE,
    pageLength = 10,
    scrollX = TRUE,
    deferRender = TRUE,
    serverSide = TRUE,
    scroller = FALSE)
  )

  callModule(caseBasedMigrantWidget, 'migrant', appStatus)

  return(NULL)
}
