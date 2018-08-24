# FUNCTIONS --------------------------------------------------------------------

FormatNumbers <- function(
  x,
  digits = 0
) {
  selNA <- is.na(x)
  res <- rep("-", length(x))
  res[!selNA] <- sprintf(paste0("%.", digits, "f"), x[!selNA])

  return(res)
}

# Format columns to string representing mid value with range
FormatRangeCols <- function(
  data,
  digits = 0
) {
  if (ncol(data) == 2) {
    res <- paste0(FormatNumbers(data[[1]], digits), " (",
                  FormatNumbers(data[[2]], digits), ")")
  } else {
    res <- paste0(FormatNumbers(data[[2]], digits), " (",
                  FormatNumbers(data[[1]], digits), ", ",
                  FormatNumbers(data[[3]], digits),")")
  }

  return(res)
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

# Formats IQR data to be displayed in the report as table
GetReportTable <- function(
  data,
  rowvar,
  colvar,
  vvars,
  mapping = colNamesMappingTable,
  digits = 0
) {
  dt <- dcast(data,
              as.formula(sprintf("%s ~ %s", rowvar, colvar)),
              value.var = vvars)

  colLevels <- levels(data[[colvar]])
  for (val in colLevels) {
    valColNames <- grep(paste0("_", val, "$"),
                        paste(vvars, val, sep = "_"),
                        value = TRUE)
    dt[, (val) := FormatRangeCols(.SD, digits = digits), .SDcols = valColNames]
  }
  dt <- dt[, c(rowvar, colLevels), with = FALSE]
  if (!is.null(mapping)) {
    mapping <- mapping[names(mapping) %in% colnames(dt)]
    setnames(dt,
             old = names(mapping),
             new = mapping)
  }

  dt <- knitr::kable(dt, align = rep("r", ncol(dt)))

  return(dt)
}

# Formats IQR data to be displayed in the report as plot
GetReportPlot <- function(
  data,
  rowvar,
  colvar,
  vvars,
  cd4YLim = NULL,
  probsStr = NULL,
  confIntervals = FALSE,
  mapping = colNamesMapping,
  colors = colorPalette
) {
  plotObj <- ggplot(data = data,
                    aes(x = as.integer(get(rowvar)),
                        y = get(vvars[1]),
                        color = get(colvar),
                        fill = get(colvar))) +
    geom_line(size = 0.5) +
    geom_point(size = 1.5) +
    scale_x_continuous(expand = c(0, 0), breaks = data[, as.integer(sort(unique(get(rowvar))))]) +
    scale_y_continuous(expand = c(0, 0)) +
    expand_limits(y = c(0, cd4YLim)) +
    scale_colour_manual(name = colvar,
                        labels = mapping,
                        values = colors) +
    scale_fill_manual(name = colvar,
                      labels = mapping,
                      values = colors) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(x = "Year", y = "Median CD4 cell count (cells/microL)")

  if (confIntervals) {
    plotObj <- plotObj +
      geom_ribbon(aes(ymin = get(vvars[2]),
                      ymax = get(vvars[3])),
                  alpha = 0.1,
                  colour = NA)
  }

  return(plotObj)
}

GetModelledQuantileData <- function(
  data,
  rowvar,
  colvar,
  vvar = "SqCD4",
  probs = c(CD4_Low = 0.25, CD4_Median = 0.5, CD4_High = 0.75)
) {
  dataList <- imputationList(split(
    data[, c(vvar, colvar, rowvar, "DY", "Imputation", "ModelWeight"),
         with = FALSE],
    by = "Imputation"))
  result <- NULL
  for (probName in names(probs)) {
    prob <- probs[probName]
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
    setnames(result, "Linpred", probName)
  }
  return(result)
}

FilterData <- function(data, colvar, badCategories)
{
  if (length(badCategories) > 0) {
    badIds <- data[get(colvar) %in% badCategories, unique(id)]
    filteredData <- data[!id %in% badIds]
    filteredData[, (colvar) := droplevels(get(colvar))]
  } else {
    filteredData <- data
  }

  return(filteredData)
}

GetModelledQuantileDataAdaptive <- function(
  data,
  rowvar,
  colvar,
  vvar,
  colNamesMapping,
  probs = c(CD4_Low = 0.25, CD4_Median = 0.5, CD4_High = 0.75),
  thresholds = c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1)
) {
  distr <- data[, .(Count = .N), by = c(colvar)]
  distr[, Perc := Count / sum(Count)]

  result <- list()
  for (threshold in thresholds) {
    badCategories <- distr[Perc < threshold, unique(get(colvar))]
    filteredData <- FilterData(data, colvar, badCategories)

    result <- suppressWarnings({
      try(GetModelledQuantileData(data = filteredData,
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

  return(list(Result = result,
              BadCategories = badCategories))
}

GetModelledTransmissionData <- function(data) {

  # mitools doesn't like factors with 0 frequency levels
  data[, Transmission := droplevels(Transmission)]

  # Fit saturated Poisson model to MI data
  dataList <- imputationList(split(data, by = "Imputation"))

  # Main model
  if (optSmoothing) {
    suppressWarnings({
      models <- with(dataList,
                     glm(Count_Val ~ as.factor(Transmission) * splines::ns(DY, df = nsdf),
                         data = data,
                         family = poisson(link = log)))
    })
  } else {
    suppressWarnings({
      models <- with(dataList,
                     glm(Count_Val ~ as.factor(Transmission) * as.factor(DY),
                         data = data,
                         family = poisson(link = log)))
    })
  }

  # Extract betas and var
  betas <- MIextract(models, fun = coefficients)
  vars <- MIextract(models, fun = vcov)

  # Rubin's rules applied by MIcombine
  t <- MIcombine(results = betas, variances = vars)
  X <- model.matrix(models$`1`$formula)
  X <- X[, names(betas$`1`)]

  # Linear predictor exponentiated to get predicted counts
  if (anyNA(coef(t))) {
    naCoef <- which(is.na(coef(t)))
    linpred <- exp(X[, -naCoef] %*% coef(t)[-naCoef])
  } else{
    linpred <- exp(X %*% coef(t))
  }

  # Manipulation to end-up with a wide-format dataframe
  pred <- cbind(dataList$imputations$`1`,
                Linpred = as.vector(linpred))
  pred <- pred[, .(DateOfDiagnosisYear, Transmission, Count_Val = Linpred)]

  return(pred)
}

# PROCESS ----------------------------------------------------------------------

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

# Unadjusted
dataOrigGender <-
  GetAggregatedData(
    data = dataOrig,
    rowvar = "DateOfDiagnosisYear",
    colvar = "Gender",
    aggrExpr =
      "{
        count <- .N
        quant <- quantile(CD4, na.rm = TRUE, probs = c(0.25, 0.5, 0.75), names = FALSE)
        list(
          Count_Val = count,
          CD4_Low = quant[1],
          CD4_Median = quant[2],
          CD4_High = quant[3])
      }")
dataOrigGender <-
  dataOrigGender[,
                 Count_Perc := Count_Val / sum(Count_Val, na.rm = TRUE) * 100,
                 by = .(DateOfDiagnosisYear)]

dataOrigTrans <-
  GetAggregatedData(
    data = dataOrig,
    rowvar = "DateOfDiagnosisYear",
    colvar = "Transmission",
    aggrExpr =
      "{
        count <- .N
        quant <- quantile(CD4, na.rm = TRUE, probs = c(0.25, 0.5, 0.75), names = FALSE)
        list(
          Count_Val = count,
          CD4_Low = quant[1],
          CD4_Median = quant[2],
          CD4_High = quant[3])
      }")
dataOrigTrans <-
  dataOrigTrans[,
                Count_Perc := Count_Val / sum(Count_Val, na.rm = TRUE) * 100,
                by = .(DateOfDiagnosisYear)]

if (cd4Present) {
  if (miPresent) {
    # Quantile regressions not possible with rare categories and discrete time
    # extrapolated - dodgy results from quantile regressions with rare
    # categories and smoothed time rare categories removed.
    dataMIGenderCD4 <-
      GetModelledQuantileDataAdaptive(data = dataMI,
                                      colvar = "Gender",
                                      rowvar = "DateOfDiagnosisYear",
                                      vvar = "SqCD4",
                                      colNamesMapping = colNamesMappingTable)$Result
    res <-
      GetModelledQuantileDataAdaptive(data = dataMI,
                                      colvar = "Transmission",
                                      rowvar = "DateOfDiagnosisYear",
                                      vvar = "SqCD4",
                                      colNamesMapping = colNamesMappingTable)

    dataMITransCD4 <- res$Result
    transBadCategories <- res$BadCategories

    cd4YLim <- GetNiceUpperLimit(max(dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
                                     dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
                                     dataMIGenderCD4$CD4_Median,
                                     dataMITransCD4$CD4_Median,
                                     na.rm = TRUE))
  } else {
    cd4YLim <- GetNiceUpperLimit(max(dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
                                     dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
                                     na.rm = TRUE))
  }
}

# Section 1: Unadjusted count data by Gender
table1 <- GetReportTable(data = dataOrigGender,
                         rowvar = "DateOfDiagnosisYear",
                         colvar = "Gender",
                         vvars = c("Count_Val", "Count_Perc"))
plot1 <- GetReportPlot(data = dataOrigGender[DateOfDiagnosisYear != "Total" & Gender != "Overall"],
                       rowvar = "DateOfDiagnosisYear",
                       colvar = "Gender",
                       vvars = "Count_Val")

# Section 2: Unadjusted Median CD4 data by Gender
table2 <- GetReportTable(data = dataOrigGender,
                         rowvar = "DateOfDiagnosisYear",
                         colvar = "Gender",
                         vvars = c("CD4_Low", "CD4_Median", "CD4_High"))
plot2 <- GetReportPlot(data = dataOrigGender[DateOfDiagnosisYear != "Total" & Gender != "Overall"],
                       rowvar = "DateOfDiagnosisYear",
                       colvar = "Gender",
                       vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                       confIntervals = TRUE)

# Section 3: Unadjusted count data by Transmission
table3 <- GetReportTable(data = dataOrigTrans,
                         rowvar = "DateOfDiagnosisYear",
                         colvar = "Transmission",
                         vvars = c("Count_Val", "Count_Perc"))
plot3 <- GetReportPlot(data = dataOrigTrans[DateOfDiagnosisYear != "Total" & Transmission != "Overall"],
                       rowvar = "DateOfDiagnosisYear",
                       colvar = "Transmission",
                       vvars = "Count_Val")

# Section 4: Unadjusted Median CD4 data by Transmission
table4 <- GetReportTable(data = dataOrigTrans,
                         rowvar = "DateOfDiagnosisYear",
                         colvar = "Transmission",
                         vvars = c("CD4_Low", "CD4_Median", "CD4_High"))
plot4 <- GetReportPlot(data = dataOrigTrans[DateOfDiagnosisYear != "Total" & Transmission != "Overall"],
                       rowvar = "DateOfDiagnosisYear",
                       colvar = "Transmission",
                       vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                       confIntervals = TRUE)

# Section 5: Adjusted count data by Transmission

# Work only with filtered data
dataMI <- FilterData(dataMI, "Transmission", transBadCategories)

dataMITrans <-
  dataMI[,
         .(Count_Val = sum(ModelWeight, na.rm = TRUE)),
         by = .(Imputation, DateOfDiagnosisYear, Transmission)]

if (miPresent) {
  dataMITrans[, DY := DateOfDiagnosisYear - min(DateOfDiagnosisYear)]
} else {
  dataMITrans[, DY := integer()]
}

dataMITrans <- GetModelledTransmissionData(data = dataMITrans)
dataMITrans <-
  dataMITrans[,
              Count_Perc := Count_Val / sum(Count_Val, na.rm = TRUE) * 100,
              by = .(DateOfDiagnosisYear)]

