# A function to return formatted 2-way tables of counts (%) and totals
fstab <- function(
  data,
  x,
  y,
  digits = 2,
  row = TRUE,
  colNamesMapping = colNamesMappingTable)
{
  freqs <- table(data[, c(x, y), with = FALSE])
  freqs <- rbind(freqs, Total = colSums(freqs, na.rm = TRUE))
  freqs[is.na(freqs)] <- 0
  rfreqs <- prop.table(freqs, 1 * row + 2 * !row)
  ftable <- data.table(rownames(freqs))
  setnames(ftable, x)
  ftable <-
    cbind(ftable,
          matrix(
            paste0(freqs, " (", formatNumbers(rfreqs * 100, digits), ")"),
            nrow = nrow(freqs),
            ncol = ncol(freqs),
            byrow = FALSE
          ),
          rowSums(freqs, na.rm = TRUE))
  setnames(ftable,
           old = seq_len(ncol(ftable))[-1],
           new = c(colnames(freqs), "Total"))
  colNamesMapping <- colNamesMapping[names(colNamesMapping) %in% colnames(ftable)]
  setnames(ftable,
           old = names(colNamesMapping),
           new = colNamesMapping)
  return(ftable)
}

GetAggregatedData <- function(
  data,
  rowvar,
  colvar,
  aggrExpr = ".(Count = .N)"
) {
  expr <- parse(text = aggrExpr)

  aggr1 <- data[, {eval(expr, envir = data)},
                by = c(rowvar, colvar)]
  aggr1 <- data[, {eval(expr, envir = data)},
                by = c(rowvar, colvar)]
  aggr2 <- data[, {eval(expr, envir = data)},
                by = c(rowvar)]
  aggr2[, (colvar) := "Overall"]
  aggr3 <- data[, {eval(expr, envir = data)},
                by = c(colvar)]
  aggr3[, (rowvar) := "Total"]
  aggr4 <- data[, {eval(expr, envir = data)}]
  aggr4[, c(rowvar, colvar) := .("Total", "Overall")]

  dt <- rbindlist(list(aggr1, aggr2, aggr3, aggr4),
                  use.names = TRUE)

  allComb <- CJ(rowvar = unique(dt[[rowvar]]),
                colvar = unique(dt[[colvar]]))
  setnames(allComb,
           old = c("rowvar", "colvar"),
           new = c(rowvar, colvar))

  dt <- dt[allComb, on = c(rowvar, colvar)]
  return(dt)
}

optReportingDelay <- as.logical(params$ReportingDelay)
optSmoothing <- as.logical(params$Smoothing)
optCD4ConfInt <- as.logical(params$CD4ConfInt)

finalDataIdx <- length(params$AdjustedData)
data <- copy(params$AdjustedData[[finalDataIdx]]$Table)

cd4Present <- data[, any(!is.na(SqCD4))]
adjTypes <- sapply(params$AdjustedData, "[[", "Type")
miPresent <- length(adjTypes[adjTypes == "MULTIPLE_IMPUTATIONS"]) > 0
rdPresent <- length(adjTypes[adjTypes == "REPORTING_DELAYS"]) > 0

# Determine last MI adjustment, if any, to get "nsdf" parameter
miAdjName <- tail(names(adjTypes[adjTypes == "MULTIPLE_IMPUTATIONS"]), 1)
if (length(miAdjName) == 1) {
  nsdf <- params$AdjustedData[[miAdjName]]$Parameters$nsdf
} else {
  nsdf <- 5L
}

# Create and initialize requried columns
if (!miPresent) {
  data[, Imputation := 0L]
}

if (rdPresent & optReportingDelay) {
  data[, ModelWeight := Weight]
} else {
  data[, ModelWeight := 1.0]
}

# A. Make manipulations ---
data[Transmission %in% c(NA, "NA", ""), Transmission := "Missing"]

# Original data
dataOrig <- data[Imputation == 0L]
dataOrig[, ':='(
  CD4 = SqCD4^2,
  Transmission = factor(Transmission),
  Gender = factor(Gender)
)]

# MI data
dataMI <- data[Imputation != 0L]
dataMI[, ':='(
  Transmission = factor(Transmission),
  Gender = factor(Gender)
)]

# B. Collapsed data for Poisson regressions ---

# Unadjusted
dataOrigCountGender <- dataOrig[, .(Count = .N),
                                by = .(DateOfDiagnosisYear, Gender)]
dataOrigCountTrans <- dataOrig[, .(Count = .N),
                               by = .(DateOfDiagnosisYear, Transmission)]

# Adjusted
dataMICountGender <- dataMI[, .(Count = sum(ModelWeight, na.rm = TRUE)),
                            by = .(Imputation, DateOfDiagnosisYear, Gender)]
dataMICountTrans <- dataMI[, .(Count = sum(ModelWeight, na.rm = TRUE)),
                           by = .(Imputation, DateOfDiagnosisYear, Transmission)]
if (miPresent) {
  dataMICountGender[, DY := DateOfDiagnosisYear - min(DateOfDiagnosisYear)]
  dataMICountTrans[, DY := DateOfDiagnosisYear - min(DateOfDiagnosisYear)]
} else {
  dataMICountGender[, DY := integer()]
  dataMICountTrans[, DY := integer()]
}

if (cd4Present) {
  maxOrigGender <-
    dataOrig[, .(MaxMedian = median(CD4, na.rm = TRUE)),
             by = .(DateOfDiagnosisYear, Gender)][, max(MaxMedian, na.rm = TRUE)]
  maxOrigTrans <-
    dataOrig[, .(MaxMedian = median(CD4, na.rm = TRUE)),
             by = .(DateOfDiagnosisYear, Transmission)][, max(MaxMedian, na.rm = TRUE)]

  if (miPresent) {
    # Quantile regressions not possible with rare categories and discrete time extrapolated - dodgy
    # results from quantile regressions with rare categories and smoothed time rare categories
    # removed.
    dataMICD4Gender <- GetMedianCD4Adaptive(dataMI, byvar = "Gender")
    dataMICD4Trans <- GetMedianCD4Adaptive(dataMI, byvar = "Transmission")

    maxMIGender <- dataMICD4Gender[[1]][, max(q1, na.rm = TRUE)]
    maxMITrans <- dataMICD4Trans[[1]][, max(q1, na.rm = TRUE)]

    cd4YLim <- GetNiceUpperLimit(max(maxOrigGender, maxOrigTrans, maxMIGender, maxMITrans))
  } else {
    cd4YLim <- GetNiceUpperLimit(max(maxOrigGender, maxOrigTrans))
  }
}

# Section 1: Unadjusted count data by Gender
genderCountData <- GetAggregatedData(data = dataOrig,
                                    rowvar = "DateOfDiagnosisYear",
                                    colvar = "Gender",
                                    aggrExpr = ".(Count = .N)")

ggplot(temp_data_orig_gender) +
  aes(x = DateOfDiagnosisYear, y = Count, color = Gender) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Number of diagnoses")  +
  scale_colour_manual(name = "Gender",
                      labels = colNamesMapping,
                      values = colorPalette) +
  scale_x_continuous(expand = c(0, 0), breaks = temp_data_orig_gender[, sort(unique(DateOfDiagnosisYear))]) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y = 0) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

genderIQRData <- GetAggregatedData(data = dataOrig,
                                   rowvar = "DateOfDiagnosisYear",
                                   colvar = "Gender",
                                   aggrExpr = "as.list(quantile(get('CD4'), na.rm = TRUE, probs = c(0.25, 0.5, 0.75)))")
genderIQRDataTable <- GetIQRDataTable(data = genderIQRData, rowvar, colvar, probsStr)
genderIQRDataPlot <- GetIQRDataPlot(data = genderIQRData[DateOfDiagnosisYear != "Total" & Gender != "Overall"],
                                    rowvar, colvar, cd4YLim, probsStr)

