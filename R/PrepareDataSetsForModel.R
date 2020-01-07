#' dt <- copy(finalData)
PrepareDataSetsForModel <- function(
  dt
) {
  dataSets <- split(dt, by = c('Imputation'))


  dt1 <- subset(
    originalData,
    select = c(
      "cd4_num", "firstcd4dateyear", "firstcd4datemonth", "firstcd4dateday", "firstcd4datequarter",
      "firstcd4dateweek", "dateofaidsdiagnosisyear", "dateofaidsdiagnosismonth",
      "dateofaidsdiagnosisday", "dateofaidsdiagnosisquarter", "dateofaidsdiagnosisweek",
      "dateofdeathyear", "dateofdiagnosisyear", "dateofdiagnosismonth", "dateofdiagnosisday",
      "dateofdiagnosisquarter", "dateofdiagnosisweek", "transmission", "gender"
    )
  )
  dt1[, ':='(
    HIVDiagnosisDate = GetDate(dateofdiagnosisyear, dateofdiagnosisquarter,
                               dateofdiagnosismonth, dateofdiagnosisweek,
                               dateofdiagnosisday),
    AIDSDiagnosisDate = GetDate(dateofaidsdiagnosisyear, dateofaidsdiagnosisquarter,
                                dateofaidsdiagnosismonth, dateofaidsdiagnosisweek,
                                dateofaidsdiagnosisday)
  )]
  dt1[, ':='(
    HIVToAIDSDaysCount = as.numeric(AIDSDiagnosisDate - HIVDiagnosisDate)
  )]

  # Define strata, can be empty
  by <- c('transmission')

  # HIV file
  hiv <- dt1[!is.na(dateofdiagnosisyear), .(count = .N), by = c('dateofdiagnosisyear', by)]
  if (length(by) > 0) {
    hiv <- dcast(
      hiv,
      as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'count'
    )
  }

  # HIV file
  aids <- dt1[!is.na(dateofaidsdiagnosisyear), .(count = .N), by = c('dateofaidsdiagnosisyear', by)]
  if (length(by) > 0) {
    aids <- dcast(
      aids,
      as.formula(sprintf('dateofaidsdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'count'
    )
  }

  # HIVAIDS file
  hivAids <- dt1[!is.na(dateofaidsdiagnosisyear) & HIVToAIDSDaysCount <= 90, .(count = .N), by = c('dateofdiagnosisyear', by)]
  if (length(by) > 0) {
    hivAids <- dcast(
      hivAids,
      as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'count'
    )
  }

}
