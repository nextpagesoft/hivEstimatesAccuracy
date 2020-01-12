#' PrepareDataSetsForModel
#'
#' Prepares data sets for HIV Model
#'
#' @param dt Input data set as data.table object. Required.
#' @param by Character vector of strata names. Optional. Def = \code{c()}
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
  by = c()
) {

  dt <- copy(dt)

  # Prepare extra details
  dt[, ':='(
    CD4Count = as.integer(cd4_num),
    HIVDiagnosisDate = GetDate(
      dateofdiagnosisyear, dateofdiagnosisquarter, dateofdiagnosismonth, dateofdiagnosisweek,
      dateofdiagnosisday
    ),
    AIDSDiagnosisDate = GetDate(
      dateofaidsdiagnosisyear, dateofaidsdiagnosisquarter, dateofaidsdiagnosismonth,
      dateofaidsdiagnosisweek, dateofaidsdiagnosisday
    ),
    FirstCD4Day = GetDate(
      firstcd4dateyear, firstcd4datequarter, firstcd4datemonth, firstcd4dateweek,
      firstcd4dateday
    )
  )]
  dt[, ':='(
    CD4Category = sprintf('HIV_CD4_%d', findInterval(CD4Count, c(0, 200, 350, 500, Inf))),
    HIVToAIDSDaysCount = as.integer(AIDSDiagnosisDate - HIVDiagnosisDate),
    HIVToFirstCD4DaysCount = as.integer(FirstCD4Day - HIVDiagnosisDate)
  )]

  # HIV file
  hiv <- dt[!is.na(dateofdiagnosisyear), .(Count = .N), by = c('dateofdiagnosisyear', by)]
  if (length(by) > 0) {
    hiv <- dcast(
      hiv,
      as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'Count'
    )
  }

  # HIV file
  aids <- dt[!is.na(dateofaidsdiagnosisyear), .(Count = .N), by = c('dateofaidsdiagnosisyear', by)]
  if (length(by) > 0) {
    aids <- dcast(
      aids,
      as.formula(sprintf('dateofaidsdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'Count'
    )
  }

  # HIVAIDS file
  hivAids <- dt[!is.na(dateofaidsdiagnosisyear) & HIVToAIDSDaysCount <= 90, .(Count = .N), by = c('dateofdiagnosisyear', by)]
  if (length(by) > 0) {
    hivAids <- dcast(
      hivAids,
      as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'Count'
    )
  }

  # CD4 files
  cd4 <- split(
    dt[!is.na(CD4Count) & HIVToFirstCD4DaysCount <= 90 & HIVToAIDSDaysCount > 90],
    by = 'CD4Category',
    sorted = TRUE
  )
  cd4 <- lapply(cd4, function(d) {
    d <- d[, .(Count = .N), by = c('dateofdiagnosisyear', by)]
    if (length(by) > 0) {
      d <- dcast(
        d,
        as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
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
