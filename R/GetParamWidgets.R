#' GetParamWidgets
#'
#' Generates a list of Shiny widgets for all parameters specified in \code{paramSpecs}.
#' It is based on \code{\link[rmarkdown]{knit_params_ask}} function.
#'
#' @param paramSpecs List of parameter specifications. Required.
#' @param params List of parameters. Optional. Default = NULL.
#' @param skipParamNames Vector of parameter names for which widgets are not to be created. Optional.
#'   Default = c().
#' @param ns Function for creating namespace. It must return a string. Optional.
#'   Default: \code{function(x) x}.
#'
#' @return List of Shiny html controls
#'
#' @examples
#' paramSpecs <- list(
#'   nimp = list(
#'     name = "nimp",
#'     label = "Number of imputations",
#'     input = "numeric",
#'     value = 2L
#'   ),
#'   chk = list(
#'     name = "chk",
#'     label = "Checkbox",
#'     input = "select",
#'     choices = c("A" = "A", "B" = "B"),
#'     multiple = TRUE
#'   )
#' )
#' GetParamWidgets(paramSpecs)
#'
#' @export
GetParamWidgets <- function(paramSpecs,
                            params = NULL,
                            skipParamNames = c(),
                            ns = function(x) x)
{
  params_configurable <- get("params_configurable", envir = asNamespace("rmarkdown"))
  params_get_control <- get("params_get_control", envir = asNamespace("rmarkdown"))
  params_label <- get("params_label", envir = asNamespace("rmarkdown"))
  params_get_input <- get("params_get_input", envir = asNamespace("rmarkdown"))
  params_value_to_ui <- get("params_value_to_ui", envir = asNamespace("rmarkdown"))

  configurable <- Filter(params_configurable, paramSpecs)
  configurable <- configurable[!names(configurable) %in% skipParamNames]

  param.ui <- function(param) {
    inputControlFn <- params_get_control(param)
    inputControlFnFormals <- names(formals(inputControlFn))

    inputId <- ns(param$name)
    label <- params_label(inputControlFn, param)

    arguments <- list(
      inputId = inputId,
      label = label)

    attrib_names <- unique(c(names(param), "value"))

    # Populate "arguments" variable
    lapply(attrib_names, function(name) {
      if (name %in% c("name", "input", "expr")) {
      } else if (name == "label") {
        arguments$label <<- label
      } else if (name == "value") {
        current_value <- param$value
        if (!is.null(params)) {
          override <- params[[param$name]]
          if (!is.null(override)) {
            current_value <- override
          }
        }
        current_value <- params_value_to_ui(inputControlFn,
                                            current_value, param$show_default)
        if ("value" %in% inputControlFnFormals) {
          arguments$value <<- current_value
        } else if ("selected" %in% inputControlFnFormals) {
          arguments$selected <<- current_value
        }
      } else if (name == "show_default") {
      } else {
        arguments[[name]] <<- if (inherits(param[[name]], "knit_param_expr")) {
          param[[name]][["value"]]
        } else {
          param[[name]]
        }
      }
    })
    uidefault <- params_value_to_ui(inputControlFn,
                                    param$value,
                                    param$show_default)
    hasDefaultValue <- function(value) {
      identical(uidefault, value)
    }

    # Get input control html
    inputControl <- NULL
    unsupported <- setdiff(names(arguments), inputControlFnFormals)
    if (length(unsupported) > 0) {
      inputControl <-
        shiny::div(class = "form-group",
                   shiny::tags$label(class = "control-label", param$name),
                   shiny::div(paste("Cannot customize the parameter \"",
                                    param$name, "\" ", "because the \"",
                                    params_get_input(param),
                                    "\" ", "Shiny control does not support: ",
                                    paste(unsupported, collapse = ", "), sep = "")))
    } else {
      inputControl <- do.call(inputControlFn, arguments)
    }

    # Get select control html
    showSelectControl <- NULL
    selectControl <- NULL
    selectInputId <- paste0("select_", param$name)
    makeSelectControl <- function(default_name, custom_name) {
      showSelectControl <<- function(current) {
        (is.null(current) || identical(current, "default"))
      }
      hasDefaultValue <<- function(value) {
        FALSE
      }
      choices <- list()
      choices[[default_name]] <- "default"
      choices[[custom_name]] <- "custom"
      selectControl <<- shiny::selectInput(inputId = selectInputId,
                                           label = label,
                                           choices = choices)
    }

    if (is.null(params[[param$name]])) {
      if (identical("Sys.time()", param$expr)) {
        makeSelectControl(paste0("now (", param$value, ")"), "Use a custom time")
      } else if (identical("Sys.Date()", param$expr)) {
        makeSelectControl(paste0("today (", param$value, ")"), "Use a custom date")
      } else if (is.null(param$value)) {
        if (!identical(inputControlFn, shiny::fileInput)) {
          makeSelectControl("Unspecified (NULL)", "Use a custom value")
        }
      }
    }

    if (!is.null(showSelectControl)) {
      widget <- selectControl
    }
    else {
      widget <- inputControl
    }

    return(widget)
  }

  widgets <- lapply(configurable, function(param) {
    param.ui(param)
  })

  # Post process stratification widget
  isStrat <- grepl("^strat", names(widgets))
  if (any(isStrat)) {
    widgets <- c(widgets[!isStrat],
                 list(strat = tags$label("Stratify by")),
                 widgets[isStrat])
  }

  return(widgets)
}
