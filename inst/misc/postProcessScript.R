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

FormatNumber <- function(
  x,
  digits = 0
) {
  return(sprintf(paste0("%.", digits, "f"), x))
}

GetAggregatedData <- function(
  data,
  rowvar,
  colvar,
  aggrExpr = ".(Count = .N)"
) {
  expr <- parse(text = aggrExpr)

  aggr1 <- data[, {eval(expr, envir = data)}, by = c(rowvar, colvar)]
  aggr2 <- data[, {eval(expr, envir = data)}, by = c(rowvar)]
  aggr3 <- data[, {eval(expr, envir = data)}, by = c(colvar)]
  aggr4 <- data[, {eval(expr, envir = data)}]

  aggr2[, (colvar) := "Overall"]
  aggr3[, (rowvar) := "Total"]
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

# Returns list:
# [[1]] - numeric values of median and q1, q3 for dateofdiagnosis and byvar (e.g. gender,
#         transmission) - longformat
# [[2]] - strings of median (iqr) for dateofdiagnosis and byvar (e.g. gender, transmission) - wide
#         format
GetModelledMedianData <- function(
  data,
  rowvar,
  colvar,
  vvar = "SqCD4",
  probs = c(0.25, 0.5, 0.75)
) {
  dataList <- imputationList(split(
    data[, c(vvar, colvar, rowvar, "DY", "Imputation", "ModelWeight"),
         with = FALSE],
    by = "Imputation"))
  result <- NULL
  for (prob in probs) {
    if (optSmoothing) {
      models <-
        with(dataList,
             rq(formula =
                  as.formula(sprintf("%s ~ %s * splines::ns(DY, df = nsdf)",
                                     vvar, colvar)),
                tau = prob,
                data = dataList$imputations,
                weights = ModelWeight,
                method = "br"))
      vars <- MIextract(models, fun = function(model) {
        diag(summary(model, covariance = TRUE)$cov)
      })
    } else {
      models <-
        with(dataList,
             rqss(formula =
                    as.formula(sprintf("%s ~ %s * as.factor(DY)", vvar, colvar)),
                  tau = prob,
                  data = dataList$imputations,
                  weights = ModelWeight,
                  method = "sfn"))
      vars <- MIextract(models, fun = function(model) {
        diag(as.matrix(summary(model, cov = TRUE)$Vcov))
      })
    }
    betas <- MIextract(models, fun = coefficients)
    t <- MIcombine(betas, vars)
    X <- model.matrix(models$`1`$formula)
    linpred <- (X %*% coef(t))^2
    pred <- cbind(dataList$imputations$`1`,
                  Linpred = as.vector(linpred))
    pred <- unique(pred[, c(rowvar, colvar, "Linpred"), with = FALSE])

    if (is.null(result)) {
      result <- copy(pred)
    } else {
      result <- cbind(result, Linpred = pred$Linpred)
    }
    setnames(result, "Linpred", as.character(prob))
  }
  return(result)
}

GetModelledMedianDataAdaptive <- function(
  data,
  rowvar,
  colvar,
  vvar,
  colNamesMapping,
  probs = c(0.25, 0.5, 0.75),
  thresholds = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1)
) {
  distr <- data[, .(Count = .N), by = c(colvar)]
  distr[, Perc := Count / sum(Count)]

  result <- list()
  for (threshold in thresholds) {
    badCategories <- distr[Perc < threshold, unique(get(colvar))]
    badIds <- data[get(colvar) %in% badCategories, unique(id)]
    reducedData <- data[!id %in% badIds]
    reducedData[, (colvar) := droplevels(get(colvar))]

    result <- suppressWarnings({
      try(GetModelledMedianData(data = reducedData,
                                colvar = colvar,
                                rowvar = rowvar,
                                vvar = vvar,
                                probs = probs),
          silent = TRUE)
      })
    if (!inherits(result, "try-error")) {
      if (length(badCategories) > 0) {
        result[["Message"]] <-
          sprintf("<p>Persons which were %s anywhere (i.e. even in one imputed dataset) are removed. Threshold of %2.2f%% of count per category within the dataset was applied.</p>",
                  paste(colNamesMapping[as.character(badCategories)],
                        collapse = ", "),
                  threshold * 100)
      }
      break
    }
  }

  return(result)
}

require(data.table)
require(ggplot2)
require(mitools)
require(quantreg)

colorPalette <- c("#69b023", "#7bbcc0", "#9d8b56", "#ce80ce", "#b23A48",
                  "#7a5980", "#63372c", "#284b63")

outputIdx <- 0
colNamesMapping <-
  c(DateOfDiagnosisYear = "Year of diagnosis",
    Total = "Total",
    Overall = "Overall",
    F = "Female",
    M = "Male",
    O = "Other",
    HAEMO = "Haemophilia",
    HETERO = "Hetero",
    IDU = "IDU",
    MTCT = "MTCT",
    MSM = "MSM",
    NOSO = "Nosocomial",
    TRANSFU = "Transfusion",
    Missing = "Missing")
colNamesMappingTable <-
  setNames(
    c(colNamesMapping[1],
      paste(colNamesMapping[2], "[N]"),
      paste(colNamesMapping[-c(1:2)], "[N (%)]")),
    names(colNamesMapping))


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
dataOrigGender <-
  dataOrig[, .(
    Count = .N,
    MedianCD4 = median(CD4, na.rm = TRUE)
  ), by = .(DateOfDiagnosisYear, Gender)]

dataOrigTrans <-
  dataOrig[, .(
    Count = .N,
    MedianCD4 = median(CD4, na.rm = TRUE)
  ), by = .(DateOfDiagnosisYear, Transmission)]

# Adjusted
dataMIGenderCount <-
  dataMI[, .(Count = sum(ModelWeight, na.rm = TRUE)),
         by = .(Imputation, DateOfDiagnosisYear, Gender)]
dataMITransCount <-
  dataMI[, .(Count = sum(ModelWeight, na.rm = TRUE)),
         by = .(Imputation, DateOfDiagnosisYear, Transmission)]

if (miPresent) {
  dataMIGenderCount[, DY := DateOfDiagnosisYear - min(DateOfDiagnosisYear)]
  dataMITransCount[, DY := DateOfDiagnosisYear - min(DateOfDiagnosisYear)]
} else {
  dataMIGenderCount[, DY := integer()]
  dataMITransCount[, DY := integer()]
}

if (cd4Present) {
  if (miPresent) {
    # Quantile regressions not possible with rare categories and discrete time
    # extrapolated - dodgy results from quantile regressions with rare
    # categories and smoothed time rare categories removed.
    dataMIGenderMedianCD4 <-
      GetModelledMedianDataAdaptive(data = dataMI,
                                    colvar = "Gender",
                                    rowvar = "DateOfDiagnosisYear",
                                    vvar = "SqCD4",
                                    colNamesMapping = colNamesMappingTable)
    dataMITransMedianCD4 <-
      GetModelledMedianDataAdaptive(data = dataMI,
                                    colvar = "Transmission",
                                    rowvar = "DateOfDiagnosisYear",
                                    vvar = "SqCD4",
                                    colNamesMapping = colNamesMappingTable)

    cd4YLim <- GetNiceUpperLimit(max(dataOrigGender$MedianCD4,
                                     dataOrigTrans$MedianCD4,
                                     dataMIGenderMedianCD4$`0.5`,
                                     dataMITransMedianCD4$`0.5`,
                                     na.rm = TRUE))
  } else {
    cd4YLim <- GetNiceUpperLimit(max(dataOrigGender$MedianCD4,
                                     dataOrigTrans$MedianCD4,
                                     na.rm = TRUE))
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

