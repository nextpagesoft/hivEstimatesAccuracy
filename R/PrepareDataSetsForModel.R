#' PrepareDataSetsForModel
#'
#' Prepares data sets for HIV Model
#'
#' @param dt Input data set as data.table object. Required.
#' @param by Character vector of strata names. Optional. Def = \code{NULL}
#'
#' @return
#' List of HIV models
#'
#' @examples
#' \dontrun{
#'   PrepareDataSetsForModel(dt, by = 'transmission')
#' }
#'
#' @export
PrepareDataSetsForModel <- function(
  dt,
  by = NULL
) {
  if (is.null(dt)) {
    return(NULL)
  }

  WorkFunc <- function(dt) {
    dt[is.na(FirstCD4Count), FirstCD4Count := DateOfDiagnosisYear]

    # Prepare extra details
    dt[, ':='(
      HIVDiagnosisDate = GetDate(
        DateOfDiagnosisYear, DateOfDiagnosisQuarter, DateOfDiagnosisMonth, DateOfDiagnosisWeek,
        DateOfDiagnosisDay
      ),
      AIDSDiagnosisDate = GetDate(
        DateOfAIDSDiagnosisYear, DateOfAIDSDiagnosisQuarter, DateOfAIDSDiagnosisMonth,
        DateOfAIDSDiagnosisWeek, DateOfAIDSDiagnosisDay
      ),
      FirstCD4Day = GetDate(
        FirstCD4DateYear, FirstCD4DateQuarter, FirstCD4DateMonth, FirstCD4DateWeek,
        FirstCD4DateDay
      )
    )]
    dt[, ':='(
      CD4Category = sprintf('HIV_CD4_%d', findInterval(FirstCD4Count, c(0, 200, 350, 500, Inf))),
      HIVToAIDSDaysCount = as.integer(AIDSDiagnosisDate - HIVDiagnosisDate),
      HIVToFirstCD4DaysCount = as.integer(FirstCD4Day - HIVDiagnosisDate)
    )]

    # HIV file
    hiv <- dt[!is.na(DateOfDiagnosisYear), .(Count = .N), keyby = c('DateOfDiagnosisYear', by)]
    setnames(hiv, old = c('DateOfDiagnosisYear'), new = c('Year'))
    if (length(by) > 0) {
      hiv <- dcast(
        hiv,
        as.formula(sprintf('Year ~ %s', paste(by, collapse = ' + '))),
        value.var = 'Count'
      )
    }

    # HIV file
    aids <- dt[
      !is.na(DateOfAIDSDiagnosisYear),
      .(Count = .N),
      keyby = c('DateOfAIDSDiagnosisYear', by)
    ]
    setnames(aids, old = c('DateOfAIDSDiagnosisYear'), new = c('Year'))
    if (length(by) > 0) {
      aids <- dcast(
        aids,
        as.formula(sprintf('Year ~ %s', paste(by, collapse = ' + '))),
        value.var = 'Count'
      )
    }

    # HIVAIDS file
    hivAids <- dt[
      !is.na(DateOfDiagnosisYear) & HIVToAIDSDaysCount <= 90,
      .(Count = .N),
      keyby = c('DateOfDiagnosisYear', by)
    ]
    setnames(hivAids, old = c('DateOfDiagnosisYear'), new = c('Year'))
    if (length(by) > 0) {
      hivAids <- dcast(
        hivAids,
        as.formula(sprintf('Year ~ %s', paste(by, collapse = ' + '))),
        value.var = 'Count'
      )
    }

    # CD4 files
    cd4 <- split(
      dt[!is.na(FirstCD4Count) & HIVToFirstCD4DaysCount <= 90 & HIVToAIDSDaysCount > 90],
      by = 'CD4Category',
      sorted = TRUE
    )
    cd4 <- lapply(cd4, function(d) {
      d <- d[, .(Count = .N), keyby = c('DateOfDiagnosisYear', by)]
      setnames(d, old = c('DateOfDiagnosisYear'), new = c('Year'))
      if (length(by) > 0) {
        d <- dcast(
          d,
          as.formula(sprintf('Year ~ %s', paste(by, collapse = ' + '))),
          value.var = 'Count'
        )
      }
      return(d)
    })

    # Dead file
    dead <- dt[!is.na(DateOfDeathYear), .(Count = .N), keyby = c('DateOfDeathYear', by)]
    setnames(dead, old = c('DateOfDeathYear'), new = c('Year'))
    if (length(by) > 0) {
      dead <- dcast(
        dead,
        as.formula(sprintf('Year ~ %s', paste(by, collapse = ' + '))),
        value.var = 'Count'
      )
    }

    dataSet <- modifyList(
      list(
        HIV = hiv,
        AIDS = aids,
        HIVAIDS = hivAids,
        Dead = dead
      ),
      cd4
    )

    # Ensure all data items have the same columns
    requiredColumns <- Reduce(
      union,
      lapply(dataSet, colnames),
      init = c()
    )

    dataSet <- lapply(
      dataSet,
      function(dt) {
        missingCols <- setdiff(requiredColumns, colnames(dt))
        if (length(missingCols) > 0) {
          dt[, (missingCols) := NA_integer_]
        }
        setcolorder(dt, requiredColumns)
        return(dt)
      }
    )

    return(
      dataSet
    )
  }

  # Process data
  dt <- copy(dt)

  if (!('Imputation' %in% colnames(dt))) {
    dt[, Imputation := 0L]
  }

  dataSets <- lapply(split(dt, by = 'Imputation'), WorkFunc)

  return(dataSets)
}
