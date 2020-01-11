#' dt <- copy(finalData)
PrepareDataSetsForModel <- function(
  dt,
  by = c('transmission')
) {
  # dataSets <- split(dt, by = c('Imputation'))

  dt1 <- subset(
    originalData,
    select = c(
      'cd4_num', 'firstcd4dateyear', 'firstcd4datemonth', 'firstcd4dateday', 'firstcd4datequarter',
      'firstcd4dateweek', 'dateofaidsdiagnosisyear', 'dateofaidsdiagnosismonth',
      'dateofaidsdiagnosisday', 'dateofaidsdiagnosisquarter', 'dateofaidsdiagnosisweek',
      'dateofdeathyear', 'dateofdiagnosisyear', 'dateofdiagnosismonth', 'dateofdiagnosisday',
      'dateofdiagnosisquarter', 'dateofdiagnosisweek', 'transmission', 'gender'
    )
  )
  dt1[, ':='(
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
  dt1[, ':='(
    CD4Category = cut(CD4Count, c(0, 200, 350, 500, Inf), right = FALSE),
    HIVToAIDSDaysCount = as.integer(AIDSDiagnosisDate - HIVDiagnosisDate),
    HIVToFirstCD4DaysCount = as.integer(FirstCD4Day - HIVDiagnosisDate)
  )]

  # HIV file
  hiv <- dt1[!is.na(dateofdiagnosisyear), .(Count = .N), by = c('dateofdiagnosisyear', by)]
  if (length(by) > 0) {
    hiv <- dcast(
      hiv,
      as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'Count'
    )
  }

  # HIV file
  aids <- dt1[!is.na(dateofaidsdiagnosisyear), .(Count = .N), by = c('dateofaidsdiagnosisyear', by)]
  if (length(by) > 0) {
    aids <- dcast(
      aids,
      as.formula(sprintf('dateofaidsdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'Count'
    )
  }

  # HIVAIDS file
  hivAids <- dt1[!is.na(dateofaidsdiagnosisyear) & HIVToAIDSDaysCount <= 90, .(Count = .N), by = c('dateofdiagnosisyear', by)]
  if (length(by) > 0) {
    hivAids <- dcast(
      hivAids,
      as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'Count'
    )
  }

  # CD4 files
  cd4 <- dt1[!is.na(CD4Count) & HIVToFirstCD4DaysCount <= 90 & HIVToAIDSDaysCount > 90]
  cd4 <- split(cd4, cd4$CD4Category)
  cd4 <- lapply(cd4, function(dt) {
    dt <- dt[, .(Count = .N), by = c('dateofdiagnosisyear', by)]
    if (length(by) > 0) {
      dt <- dcast(
        dt,
        as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
        value.var = 'Count'
      )
    }
  })
}
