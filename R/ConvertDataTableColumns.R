#' ConvertDataTableColumns
#'
#' Converts columns of a data.table object to specified classes.
#' The transformation is made by reference, thus the changes are made directly to the object
#' passed in and no copies are made.
#'
#' @param object A data.table object whose column classes are to be converted
#' @param columnDefs A list of column class definitions in a form "[ColumnName] = [ColumnClass]"
#' @param levelsFunc Function used to initialize levels of factor columns. (def: NULL)
#' @param ... Additional arguments passed to \code{levelsFunc}.
#'
#' @return
#' NULL
#'
#' @examples
#' object <- data.table::data.table(RecordId = c("1", "2"), CD4 = c("233", "459"))
#' # Column definitions as named vector
#' columnDefs <- c(RecordId = "integer", CD4 = "numeric")
#' # Check class of columns before conversion
#' sapply(object, class)
#' ConvertDataTableColumns(object, columnDefs)
#' # Check class of columns after conversion
#' sapply(object, class)
#'
#' @export
ConvertDataTableColumns <- function(object, columnDefs, levelsFunc = NULL, ...)
{
  if (!is.data.table(object))
    stop("Input object must be of class data.table")

  objectColNames <- colnames(object)

  # Iterate over defined transformations
  # columnName <- names(columnDefs)[1]
  for (columnName in names(columnDefs)) {

    columnDef <- columnDefs[[columnName]]

    # Transformation as new class name
    if (is.character(columnDef)) {
      transFunc <- switch(tolower(columnDef),
                          logical = as.logical,
                          integer = as.integer,
                          single = as.single,
                          double = as.double,
                          numeric = as.numeric,
                          string = as.character,
                          character = as.character,
                          factor = factor,
                          NULL)
    } else if (is.function(columnDef)) {
      transFunc <- columnDef
    } else {
      transFunc <- NULL
    }

    # Apply the transformation if both:
    #   a) specified,
    #   b) the transformed column exists
    if (!is.null(transFunc) & columnName %in% objectColNames) {
      # Apply the transformation to the data.table by reference (directly on the object, no copies made)
      if (is.character(columnDef)) {
        if (columnDef == "factor" & !is.null(levelsFunc)) {
          object[, eval(columnName) := transFunc(get(columnName), levels = levelsFunc(columnName, ...))]
          next
        }
      }
      object[, eval(columnName) := transFunc(get(columnName))]
    }
  }

  # Do not return anything and do not print anything
  invisible(NULL)
}
