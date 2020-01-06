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

  by <- c('transmission')
  hiv <- dt1[, .(count = .N), by = c('dateofdiagnosisyear', by)]

  if (length(by) > 0) {
    hiv <- dcast(
      hiv,
      as.formula(sprintf('dateofdiagnosisyear ~ %s', paste(by, collapse = ' + '))),
      value.var = 'count'
    )
  }

}
