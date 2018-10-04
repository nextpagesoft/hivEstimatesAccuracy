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
stratVarNames <- c()

# Run mice adjustment before RD
runMice <- FALSE

# B) PROCESS DATA --------------------------------------------------------------

inputDataFilePath <- "/media/sf_VirtualBox_Share/dummy_miss1.zip"
# inputDataFilePath <- "C:/Users/mrosinska/Desktop/programy/ecdc_adjustment/TESSy_new/PLtest.csv"
# inputDataFilePath <- "C:/Users/mrosinska/Documents/projekty/ecdc adjustment/data2017/EL_imp.csv"

# Read original data
originalData <- ReadDataFile(inputDataFilePath)

# Apply attribute mapping
attrMapping <- GetPreliminaryAttributesMapping(originalData)
if (is.null(attrMapping[["FirstCD4Count"]])) {
  attrMapping[["FirstCD4Count"]] <- "cd4_num"
}
inputData <- ApplyAttributesMapping(originalData,
                                    attrMapping,
                                    GetPreliminaryDefaultValues())

# Pre-process data
inputData <- PreProcessInputData(inputData = inputData)

# Apply Origin mapping
distr <- GetOriginDistribution(inputData$Table)
map <- GetOriginGroupingMap(type = "REPCOUNTRY + UNK + OTHER", distr)
inputData <- ApplyOriginGroupingMap(inputData, map)

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

# Work on a copy of input data
compData <- copy(inputData)

# Add dummy "Imputation" column if not found
isOriginalData <- !("Imputation" %in% colnames(compData))
if (isOriginalData) {
  compData[, Imputation := 0L]
}

# Make sure the strata columns exist in the data
stratVarNames <- stratVarNames[stratVarNames %in% colnames(compData)]
stratVarNamesTrend <- union("DateOfDiagnosisYear",
                            stratVarNames)

# Create intermediate variables
compData[, ":="(
  NotificationTime = DateOfNotificationYear + 1/4 * DateOfNotificationQuarter,
  DiagnosisTime = DateOfDiagnosisYear + 1/4 * DateOfDiagnosisQuarter
)]

# Create dimensions to match the weights later
outputData <- copy(compData)
outputData[, MaxNotificationTime := max(NotificationTime, na.rm = TRUE),
           by = .(ReportingCountry)]
outputData[, VarT := 4 * (pmin(MaxNotificationTime, endQrt) - DiagnosisTime) + 1]

# Filter
compData <- compData[!is.na(DiagnosisTime) & !is.na(NotificationTime)]
compData <- compData[DiagnosisTime >= max(startYear + 0.25,
                                          min(NotificationTime, na.rm = TRUE)) &
                       NotificationTime <= endQrt]
compData[, VarX := 4 * (NotificationTime - DiagnosisTime)]
compData[VarX < 0, VarX := NA]
compData[, ":="(
  MinNotificationTime = min(NotificationTime, na.rm = TRUE),
  MaxNotificationTime = max(NotificationTime, na.rm = TRUE)
), by = .(ReportingCountry)]
compData[, ":="(
  VarT = 4 * (pmin(MaxNotificationTime, endQrt) - DiagnosisTime) + 1,
  Tf = 4 * (pmin(MaxNotificationTime, endQrt) - pmax(MinNotificationTime, startYear)),
  ReportingDelay = 1L
)]
compData[, ":="(
  VarXs = Tf - VarX,
  VarTs = Tf - VarT
)]
# NOTE: Otherwise survival model complains
compData <- compData[VarXs > VarTs]

# ------------------------------------------------------------------------
# Prepare diagnostic table based on original data
model <- compData[Imputation == 0L,
                  Surv(time = VarTs,
                       time2 = VarXs,
                       event = ReportingDelay)]

# Defining univariate models
univFormulas <- lapply(
  stratVarNamesTrend,
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
    res <- cbind(Predictor = rownames(y$conf.int),
                 res)
    return(res)}))

# ------------------------------------------------------------------------
# RD estimation with time trend

# Define parameters
lastYear <- compData[, max(DateOfDiagnosisYear)]
years <- (lastYear - 4):lastYear
tGroups <- 1:3
imputations <- compData[, sort(unique(Imputation))]
formula <- as.formula(sprintf("Surv(time = VarTs, time2 = VarXs, event = ReportingDelay) ~ (%s):strata(tgroup)",
                              paste(stratVarNamesTrend, collapse = " + ")))
cuts <- compData[, c(max(VarXs) - 10,
                     max(VarXs) - 3)]

# Run fitting per imputation separately
fitStratum <- list()
for (imputation in imputations) {
  compDataSplit <- setDT(survSplit(
    formula = Surv(time = VarTs, time2 = VarXs, event = ReportingDelay) ~ .,
    data = compData[Imputation == imputation],
    cut = cuts,
    episode = "tgroup"))

  fitCox <- coxph(formula,
                  data = compDataSplit)

  estFrame <- CJ(DateOfDiagnosisYear = years,
                 tgroup = tGroups)

  estCov <- na.omit(unique(compData[, ..stratVarNames]))
  if (nrow(estCov) > 0) {
    estFrame[, MergeDummy := 1]
    estCov[, MergeDummy := 1]
    estFrame <- estFrame[estCov,
                         on = .(MergeDummy),
                         allow.cartesian = TRUE]
    estFrame[, MergeDummy := NULL]
  }

  estFrame[, ":="(
    Id = rep(seq_len(.N/length(tGroups)), each = length(tGroups)),
    VarTs = ifelse(tgroup == 1,  0, ifelse(tgroup == 2, 52, ifelse(tgroup == 3, 59, NA))),
    VarXs = ifelse(tgroup == 1, 52, ifelse(tgroup == 2, 59, ifelse(tgroup == 3, 62, NA))),
    ReportingDelay = 0
  )]

  fit <- survfit(fitCox,
                 newdata = estFrame,
                 id = Id)

  fitStratumImp <- data.table(
    Imputation = imputation,
    Delay = fit$time,
    P = fit$surv,
    Weight = 1/fit$surv,
    Var = fit$std.err^2,
    unique(estFrame[, ..stratVarNamesTrend])[rep(seq_len(.N), fit$strata)])
  fitStratumImp[, VarT := max(Delay) - Delay]

  # Store this imputation results
  fitStratum[[as.character(imputation)]] <- fitStratumImp[VarT >= 0]
}
fitStratum <- rbindlist(fitStratum)

# Merge P, Weight and Var with outputData object
mergeVars <- union(stratVarNamesTrend,
                   c("VarT", "Imputation"))
outputData[, ":="(
  DateOfDiagnosisYearOrig = DateOfDiagnosisYear,
  DateOfDiagnosisYear = pmax.int(lastYear - 4, DateOfDiagnosisYear),
  Source = ifelse(Imputation == 0, "Reported", "Imputed")
)]
outputData <- merge(outputData,
                    fitStratum[, c(..mergeVars, "P", "Weight", "Var")],
                    by = mergeVars,
                    all.x = TRUE)
outputData[, MissingData := is.na(Weight)]
# outputData[, MissingData := FALSE]
outputData[is.na(Weight), Weight := 1]
outputData[is.na(P), P := 1]
outputData[is.na(Var), Var := 0]
outputData[, ":="(
  DateOfDiagnosisYear = DateOfDiagnosisYearOrig,
  DateOfDiagnosisYearOrig = NULL
)]

# ------------------------------------------------------------------------

# Get distribution object as artifact
varNames <- setdiff(colnames(fitStratum),
                    c("Delay", "P", "Weight", "Var", "VarT"))
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
                      by = c(mergeVars, "Source", "MissingData")]

# Compute estimated count and its variance
agregat[, ":="(
  EstCount = Count * Weight,
  EstCountVar = (Count * (Count + 1) / P^4 * Var) + Count * (1 - P) / P^2
)]

# D) TOTAL PLOT ----------------------------------------------------------
totalPlotData <- GetRDPlotData(data = agregat,
                               by = c("MissingData", "Source", "Imputation",
                                      "DateOfDiagnosisYear"))
setorderv(totalPlotData, c("MissingData", "DateOfDiagnosisYear"))
totalPlot <- GetRDPlots(plotData = totalPlotData,
                        isOriginalData = isOriginalData)

reportTableData <- dcast(totalPlotData,
                         DateOfDiagnosisYear + EstCount + LowerEstCount +
                           UpperEstCount ~ MissingData,
                         value.var = "Count",
                         fun.aggregate = sum)
setnames(reportTableData,
         old = c("FALSE", "TRUE"),
         new = c("Reported", "MissingData"))
reportTableData[, lapply(.SD, sum),
                by = DateOfDiagnosisYear,
                .SDcols = setdiff(colnames(reportTableData),
                                  "DateOfDiagnosisYear")]
reportTableData[, ":="(
  EstUnreported = EstCount - (Reported + MissingData),
  LowerEstUnreported = LowerEstCount - (Reported + MissingData),
  UpperEstUnreported = UpperEstCount - (Reported + MissingData)
)]
setcolorder(reportTableData,
            c("DateOfDiagnosisYear", "MissingData", "Reported",
              "EstUnreported", "EstCount", "LowerEstCount", "UpperEstCount"))

# E) STRATIFIED PLOT (OPTIONAL) ------------------------------------------
if (length(stratVarNames) > 0) {
  # Stratification
  colNames <- union(c("MissingData", "Source", "DateOfDiagnosisYear", "Count", "EstCount",
                      "EstCountVar"),
                    c(stratVarNamesTrend, "Imputation"))
  # Keep only required columns, convert data to "long" format...
  agregatLong <- melt(agregat[, ..colNames],
                      measure.vars = stratVarNames,
                      variable.name = "Stratum",
                      value.name = "StratumValue")

  stratPlotListData <- GetRDPlotData(data = agregatLong,
                                     by = c("MissingData", "Source", "Imputation",
                                            "DateOfDiagnosisYear", "Stratum", "StratumValue"))
  stratPlotList <- lapply(stratVarNames,
                          GetRDPlots,
                          plotData = stratPlotListData,
                          isOriginalData = isOriginalData)

  names(stratPlotList) <- stratVarNames
}

# Keep only columns present in the input object plus the weight
outColNames <- union(colnames(inputData),
                     c("VarT", "Weight"))
outputData <- outputData[, ..outColNames]

artifacts <- list(OutputPlotTotal = totalPlot,
                  OutputPlotTotalData = totalPlotData,
                  OutputPlotStrat = stratPlotList,
                  OutputPlotStratData = stratPlotListData,
                  ReportTableData = reportTableData,
                  RdDistribution = rdDistribution,
                  UnivAnalysis = univAnalysis)
