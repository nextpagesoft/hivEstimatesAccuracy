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
#' PrepareDataSetsForModel(dt, by = 'transmission')
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
    dt <- copy(dt)

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
    hiv <- dt[!is.na(DateOfDiagnosisYear), .(Count = .N), by = c('DateOfDiagnosisYear', by)]
    if (length(by) > 0) {
      hiv <- dcast(
        hiv,
        as.formula(sprintf('DateOfDiagnosisYear ~ %s', paste(by, collapse = ' + '))),
        value.var = 'Count'
      )
    }

    # HIV file
    aids <- dt[!is.na(DateOfAIDSDiagnosisYear), .(Count = .N), by = c('DateOfAIDSDiagnosisYear', by)]
    if (length(by) > 0) {
      aids <- dcast(
        aids,
        as.formula(sprintf('DateOfAIDSDiagnosisYear ~ %s', paste(by, collapse = ' + '))),
        value.var = 'Count'
      )
    }

    # HIVAIDS file
    hivAids <- dt[!is.na(DateOfDiagnosisYear) & HIVToAIDSDaysCount <= 90, .(Count = .N), by = c('DateOfDiagnosisYear', by)]
    if (length(by) > 0) {
      hivAids <- dcast(
        hivAids,
        as.formula(sprintf('DateOfDiagnosisYear ~ %s', paste(by, collapse = ' + '))),
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
      d <- d[, .(Count = .N), by = c('DateOfDiagnosisYear', by)]
      if (length(by) > 0) {
        d <- dcast(
          d,
          as.formula(sprintf('DateOfDiagnosisYear ~ %s', paste(by, collapse = ' + '))),
          value.var = 'Count'
        )
      }
      return(d)
    })

    return(
      modifyList(
        list(
          HIV = hiv,
          AIDS = aids,
          HIVAIDS = hivAids
        ),
        cd4
      )
    )
  }

  if ('Imputation' %in% colnames(dt)) {
    results <- lapply(split(dt, by = 'Imputation'), WorkFunc)
  } else {
    results <- WorkFunc(dt)
  }

  return(results)
}
