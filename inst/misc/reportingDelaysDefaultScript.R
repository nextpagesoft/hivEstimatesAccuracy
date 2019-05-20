# Load libraries
library(hivEstimatesAccuracy)
library(data.table)
library(survival)


# A) SETUP ---------------------------------------------------------------------

# Start year
startYear <- 2000L

# End quarter
endQrt <- 2017.25

# Stratifiation columns
# stratVarNames <- c("Gender", "Transmission")
stratVarNames <- c("Transmission")

# Run mice adjustment before RD
runMice <- FALSE

# B) PROCESS DATA --------------------------------------------------------------

inputDataFilePath <- "~/share/dummy2019_exclUK.zip"
# inputDataFilePath <- "C:/Users/mrosinska/Desktop/programy/ecdc_adjustment/TESSy_new/PLtest.csv"
# inputDataFilePath <- "C:/Users/mrosinska/Documents/projekty/ecdc adjustment/data2017/EL_imp.csv"

# Read original data
originalData <- ReadDataFile(inputDataFilePath)

# Apply attribute mapping
attrMapping <- GetPreliminaryAttributesMapping(originalData)
if (is.null(attrMapping[["FirstCD4Count"]])) {
  attrMapping[["FirstCD4Count"]] <- "cd4_num"
}
if (is.null(attrMapping[["RecordId"]])) {
  attrMapping[["RecordId"]] <- "Identyfikator"
}
if (is.null(attrMapping[["Age"]])) {
  attrMapping[["Age"]] <- "WiekHIVdor"
}
inputData <- ApplyAttributesMapping(originalData,
                                    attrMapping,
                                    GetPreliminaryDefaultValues())

# Pre-process data
inputData <- PreProcessInputDataBeforeSummary(inputData = inputData)

# Apply Origin mapping
distr <- GetOriginDistribution(inputData$Table)
map <- GetOriginGroupingMap(type = "REPCOUNTRY + UNK + OTHER", distr)
inputData <- ApplyOriginGroupingMap(inputData, map)

PreProcessInputDataBeforeAdjustments(inputData = inputData$Table)

# Apply mice adjustment (optional)
if (runMice) {
  adjustmentNames <- c("Multiple Imputation using Chained Equations - MICE")
  adjustmentFilePaths <- GetAdjustmentSpecFileNames()
  adjustmentSpecs <-
    setNames(lapply(adjustmentNames,
                    function(adjName) GetListObject(adjustmentFilePaths[adjName])),
             adjustmentNames)
  adjustedData <- RunAdjustments(data = inputData$Table,
                                 adjustmentSpecs = adjustmentSpecs)
  inputData <- adjustedData$`1. Multiple Imputation using Chained Equations - MICE`
}

inputData <- inputData$Table

# C) RD ADJUSTMENT -------------------------------------------------------------

# Work on a copy
compData <- copy(inputData)

# Initialize output data
outputData <- copy(inputData)

# Separator used for creating a composite of stratum columns. Should not occur in the stratum
# values.
stratSep <- "_"

# B) PROCESS DATA ------------------------------------------------------------------------------

# Add dummy "Imputation" column if not found
isOriginalData <- !("Imputation" %in% colnames(compData))
if (isOriginalData) {
  compData[, Imputation := 0L]
}

# Make sure the strata columns exist in the data
stratVarNames <- stratVarNames[stratVarNames %in% colnames(compData)]
stratVarNamesImp <- union(c("Imputation", "ReportingCountry"),
                          stratVarNames)

# Create a stratum variable - will be used to merge with the estimations,
# takes missing categories as separate categories
compData[, Stratum := strata(.SD,
                             shortlabel = TRUE,
                             sep = stratSep,
                             na.group = TRUE),
         .SDcols = stratVarNamesImp]

# Create dimensions to match the weights later
outputData <- copy(compData)
outputData[, VarT := 4 * (pmin.int(MaxNotificationTime, endQrt) - DiagnosisTime) + 1]

# Filter
compData <- compData[!is.na(VarX)]
compData[is.na(DiagnosisTime), DiagnosisTime := DateOfDiagnosisYear + 0.25]
compData[is.na(NotificationTime), NotificationTime := DiagnosisTime + VarX / 4]

compData <- compData[VarX >= 0 &
                       DiagnosisTime >= (startYear + 0.25) &
                       NotificationTime <= endQrt]

compData[, ":="(
  VarT = 4 * (pmin.int(MaxNotificationTime, endQrt) - DiagnosisTime),
  Tf = 4 * (pmin.int(MaxNotificationTime, endQrt) - pmax.int(min(DiagnosisTime), startYear + 0.25)),
  ReportingDelay = 1L
)]
compData[, ":="(
  VarXs = Tf - VarX,
  VarTs = Tf - VarT
)]
# NOTE: Otherwise survival model complains
compData <- compData[VarXs > VarTs]
compData <- droplevels(compData)

totalPlot <- NULL
totalPlotData <- NULL
rdData <- NULL
stratPlotList <- NULL
stratPlotListData <- NULL
rdDistribution <- NULL
univAnalysis <- NULL
if (nrow(compData) > 0) {

  # ------------------------------------------------------------------------
  # Prepare diagnostic table based on original data

  mostPrevGender <- compData[!is.na(Gender), .N, by = .(Gender)][frank(-N, ties.method = "first") == 1, as.character(Gender)]
  mostPrevTrans <- compData[!is.na(Transmission), .N, by = .(Transmission)][frank(-N, ties.method = "first") == 1, as.character(Transmission)]
  mostPrevRegion <- compData[!is.na(GroupedRegionOfOrigin), .N, by = .(GroupedRegionOfOrigin)][frank(-N, ties.method = "first") == 1, as.character(GroupedRegionOfOrigin)]

  if (!IsEmptyString(mostPrevGender)) {
    compData[, Gender := relevel(Gender, ref = mostPrevGender)]
  }
  if (!IsEmptyString(mostPrevTrans)) {
    compData[, Transmission := relevel(Transmission, ref = mostPrevTrans)]
  }
  if (!IsEmptyString(mostPrevRegion)) {
    compData[, GroupedRegionOfOrigin := relevel(GroupedRegionOfOrigin, ref = mostPrevRegion)]
  }

  model <- compData[Imputation == 0L,
                    Surv(time = VarTs,
                         time2 = VarXs,
                         event = ReportingDelay)]

  # Defining univariate models
  univFormulas <- lapply(
    stratVarNames,
    function(x) as.formula(sprintf("model ~ %s", x)))

  # Applying univariate models
  univModels <- lapply(
    univFormulas,
    function(x) coxph(x, data = compData[Imputation == 0L]))

  # Extract results of univariable analysis (whether particular covariates
  # are associated with RD)
  univAnalysis <- rbindlist(lapply(
    univModels,
    function(x) {
      y <- summary(x)
      z <- cox.zph(x)
      res <- merge(as.data.table(y$conf.int),
                   as.data.table(y$coefficients))
      res <- cbind(res,
                   as.data.table(z$table[rownames(z$table) != "GLOBAL", "p", drop = FALSE]))
      res[, lapply(.SD, signif, 2), .SDcols = colnames(res)]
      setnames(res, c("HR", "1/HR", "HR.lower.95",
                      "HR.upper.95", "Beta", "SE.Beta",
                      "Z", "P.value", "Prop.assumpt.p"))

      if (!is.null(x$xlevels)) {
        varName <- names(x$xlevels)[1]
        refLevel <- x$xlevels[[varName]][1]
        compLevels <- x$xlevels[[varName]][-1]
        predictor <- sprintf("%s (%s vs %s)", varName, compLevels, refLevel)
      } else {
        predictor <- rownames(y$conf.int)
      }

      res <- cbind(Predictor = predictor,
                   res)
      return(res)}))

  # ------------------------------------------------------------------------
  # RD estimation without time trend

  model <- compData[, Surv(time = VarTs,
                           time2 = VarXs,
                           event = ReportingDelay)]
  fit <- compData[, survfit(model ~ Stratum)]
  if (is.null(fit$strata) & length(levels(compData$Stratum)) == 1) {
    strata <- c(1L)
    names(strata) <- paste0("Stratum=", levels(outputData$Stratum)[1])
  } else {
    strata <- fit$strata
  }

  # Recreating stratum variables to assign them to the delay distribution dataset
  fitStratum <- data.table(
    Delay = fit$time,
    P = fit$surv,
    Weight = 1/fit$surv,
    Var = fit$std.err^2,
    Stratum = factor(rep(seq_along(strata), strata),
                     labels = levels(compData$Stratum)))
  fitStratum[, (stratVarNamesImp) := tstrsplit(Stratum, stratSep)]
  fitStratum[, VarT := max(Delay) - Delay]
  fitStratum <- fitStratum[VarT >= 0]
  # Convert "NA" to NA
  fitStratum[, (stratVarNamesImp) := lapply(.SD, function(x) ifelse(x == "NA", NA_character_, x)),
             .SDcols = stratVarNamesImp]

  # Create final output object
  outputData[fitStratum[, c("Stratum", "VarT", "Weight", "P", "Var"), with = FALSE],
             ":="(
               Weight = Weight,
               P = P,
               Var = Var
             ), on = .(VarT, Stratum)]
  outputData[, ":="(
    Source = ifelse(Imputation == 0, "Reported", "Imputed"),
    MissingData = VarX != 0 & (is.na(Weight) | is.infinite(Weight))
  )]
  outputData[MissingData == TRUE | VarX == 0, ":="(
    Weight = 1,
    P = 1
  )]
  outputData[is.na(Var), Var := 0]

  # ------------------------------------------------------------------------

  # Get distribution object as artifact
  varNames <- setdiff(colnames(fitStratum),
                      c("Delay", "P", "Var", "Stratum", "VarT"))
  rdDistribution <- fitStratum[VarT > 0,
                               union(varNames, c("VarT", "P", "Weight", "Var")),
                               with = FALSE]
  setnames(rdDistribution,
           old = "VarT",
           new = "Quarter")
  setorderv(rdDistribution, union(varNames, "Quarter"))

  # Aggregate and keep only required dimensions
  agregat <- outputData[, .(Count = .N,
                            P = mean(P),
                            Weight = mean(Weight),
                            Var = mean(Var)),
                        by = eval(union(stratVarNamesImp,
                                        c("Source", "MissingData", "DateOfDiagnosisYear")))]

  # Compute estimated count and its variance
  agregat[, ":="(
    EstCount = Count * Weight,
    EstCountVar = (Count * (Count + 1) / P^4 * Var) + Count * (1 - P) / P^2
  )]

  # C) TOTAL PLOT ----------------------------------------------------------------------------------
  totalPlotData <- GetRDPlotData(data = agregat,
                                 by = c("MissingData", "Source", "Imputation",
                                        "DateOfDiagnosisYear"))
  setorderv(totalPlotData, c("MissingData", "DateOfDiagnosisYear"))
  totalPlot <- GetRDPlots(plotData = totalPlotData,
                          isOriginalData = isOriginalData)

  reportTableData <- dcast(totalPlotData[Source == ifelse(isOriginalData, "Reported", "Imputed")],
                           DateOfDiagnosisYear + EstCount +
                             LowerEstCount + UpperEstCount ~ MissingData,
                           value.var = "Count",
                           fun.aggregate = sum)
  if ("TRUE" %in% colnames(reportTableData)) {
    setnames(reportTableData, old = "TRUE", new = "RDWeightNotEstimated")
  } else {
    reportTableData[, RDWeightNotEstimated := 0]
  }
  if ("FALSE" %in% colnames(reportTableData)) {
    setnames(reportTableData, old = "FALSE", new = "RDWeightEstimated")
  } else {
    reportTableData[, RDWeightEstimated := 0]
  }

  reportTableData <- reportTableData[, lapply(.SD, sum),
                                     by = DateOfDiagnosisYear,
                                     .SDcols = setdiff(colnames(reportTableData),
                                                       "DateOfDiagnosisYear")]
  reportTableData[, Reported := RDWeightEstimated + RDWeightNotEstimated]
  reportTableData[, ":="(
    EstUnreported = EstCount - Reported,
    LowerEstUnreported = LowerEstCount - Reported,
    UpperEstUnreported = UpperEstCount - Reported
  )]
  setcolorder(reportTableData,
              c("DateOfDiagnosisYear",
                "Reported", "RDWeightEstimated", "RDWeightNotEstimated",
                "EstUnreported", "LowerEstUnreported", "UpperEstUnreported",
                "EstCount", "LowerEstCount", "UpperEstCount"))

  # D) STRATIFIED PLOT (OPTIONAL) ------------------------------------------------------------------
  if (length(stratVarNames) > 0) {
    # Stratification
    colNames <- union(c("MissingData", "Source", "DateOfDiagnosisYear", "Count", "EstCount",
                        "EstCountVar"),
                      stratVarNamesImp)
    # Keep only required columns, convert data to "long" format...
    agregatLong <- melt(agregat[, ..colNames],
                        measure.vars = stratVarNames,
                        variable.name = "Stratum",
                        value.name = "StratumValue")
    agregatLong[, StratumValue := factor(StratumValue)]

    stratPlotListData <- GetRDPlotData(data = agregatLong,
                                       by = c("MissingData", "Source", "Imputation",
                                              "DateOfDiagnosisYear", "Stratum", "StratumValue"))
    stratPlotList <- lapply(stratVarNames,
                            GetRDPlots,
                            plotData = stratPlotListData[MissingData == FALSE],
                            isOriginalData = isOriginalData)

    names(stratPlotList) <- stratVarNames
  }
} else {
  outputData[, Weight := 1]
}

# Keep only columns present in the input object plus the weight
outColNames <- union(colnames(inputData),
                     c("VarT", "Stratum", "Weight"))
outputData <- outputData[, ..outColNames]

artifacts <- list(OutputPlotTotal = totalPlot,
                  OutputPlotTotalData = totalPlotData,
                  OutputPlotStrat = stratPlotList,
                  OutputPlotStratData = stratPlotListData,
                  ReportTableData = reportTableData,
                  RdDistribution = rdDistribution,
                  UnivAnalysis = univAnalysis)
