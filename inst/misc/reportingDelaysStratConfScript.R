# Load libraries
library(hivEstimatesAccuracy)
library(data.table)
library(ggplot2)

# Settings

# Color palette for the plots
# First color: background of confidence interval for the estimated count
# Second color: estimated count
# Third color: reported count
colorPalette <- c("#AAAAAA", "#E69F00", "#56B4E9")

# Separator used for creating a composite of stratum columns. Should not occur in the stratum values.
stratSep <- "_"

# Stratifiation columns
stratVarNames <- c()

# Start year
startYear <- 2000L

# End quarter
endQrt <- 2017.25

# Input data path
inputDataFilePath <- "d:/Drive/Projects/19. PZH/Scripts/Received/20171013/InputData/EL.csv"
# inputDataFilePath <- "C:/Users/mrosinska/Desktop/programy/ecdc_adjustment/TESSy_new/PLtest.xls"

# A) READ INPUT DATA ------------------------------------------------------------------------------

# Read input data
originalData <- ReadDataFile(inputDataFilePath)

# Get preliminary attributes mapping
attrMapping <- GetPreliminaryAttributesMapping(originalData)
# Adjust mapping
attrMapping[["FirstCD4Count"]] <- "cd4_num"

# Pre-process data
inputData <- ApplyAttributesMapping(originalData, attrMapping)
inputDataValidityInfo <- ValidateInputData(inputData)
inputData <- PreProcessInputData(inputData)

# B) PROCESS DATA ---------------------------------------------------------------------------------

# Create intermediate variables
inputData[, ":="(
  NotificationTime = DateOfNotificationYear + 1/4 * DateOfNotificationQuarter,
  DiagnosisTime = DateOfDiagnosisYear + 1/4 * DateOfDiagnosisQuarter
)]

# Filter
inputData <- inputData[DiagnosisTime > max(startYear, min(NotificationTime))
                       & NotificationTime <= endQrt]

inputData[, VarX := 4 * (NotificationTime - DiagnosisTime)]
inputData[VarX < 0, VarX := NA]
inputData[, ":="(
  MinNotificationTime = min(NotificationTime),
  MaxNotificationTime = max(NotificationTime)
), by = .(ReportingCountry)]

inputData[, ":="(
  VarT = 4 * (pmin(MaxNotificationTime, endQrt) - DiagnosisTime) + 1,
  Tf = 4 * (pmin(MaxNotificationTime, endQrt) - pmax(MinNotificationTime, startYear)),
  ReportingDelay = 1L
)]
inputData[, ":="(
  VarXs = Tf - VarX,
  VarTs = Tf - VarT
)]
# NOTE: Otherwise survival model complains
inputData <- inputData[VarXs > VarTs]

# Create a stratum variable - will be used to merge with the estimations,
# takes missing categories as separate categories
if (length(stratVarNames) > 0) {
  inputData[, Stratum := survival::strata(.SD, shortlabel = TRUE, sep = stratSep, na.group = TRUE),
            .SDcols = stratVarNames]
} else {
  inputData[, Stratum := factor("Total")]
}

# Fit a stratified survival model
model <- inputData[, survival::Surv(time = VarTs,
                                    time2 = VarXs,
                                    event = ReportingDelay)]
fit <- inputData[, survival::survfit(model ~ Stratum)]
if (is.null(fit$strata)) {
  strata <- 1L
} else {
  strata <- fit$strata
}

# Aggregate and keep only required dimensions
agregat <- inputData[, .(Count = .N),
                     by = .(DiagnosisTime,
                            YearOfDiagnosis = DateOfDiagnosisYear,
                            ReportingCountry,
                            VarT,
                            Stratum)]
# Recreating stratum variables to assign them to the delay distribution dataset
fitStratum <- data.table(
  Delay = fit$time,
  P = fit$surv,
  PL = fit$lower,
  PU = fit$upper,
  Var = fit$std.err^2,
  Stratum = factor(rep(seq_along(strata), strata),
                   labels = levels(inputData$Stratum)))
if (length(stratVarNames) > 0) {
  fitStratum[, (stratVarNames) := tstrsplit(Stratum, stratSep)]
}
fitStratum[, VarT := max(Delay) - Delay]
fitStratum <- fitStratum[VarT >= 0]
# Merge fit data
agregat <- fitStratum[agregat,
                      on = .(VarT, Stratum)]

# Compute estimated count and its variance
agregat[, ":="(
  EstCount = Count / P,
  LowerEstCount = Count / PU,
  UpperEstCount = Count / PL
)]

# c) TOTAL PLOT -----------------------------------------------------------------------------------

# Aggregating by year
agregatByYear <- agregat[, .(Count = sum(Count),
                             EstCount = sum(EstCount),
                             LowerEstCount = sum(LowerEstCount),
                             UpperEstCount = sum(UpperEstCount)),
                         by = .(YearOfDiagnosis)]

agregatPlotYr <- ggplot(agregatByYear, aes(x = YearOfDiagnosis)) +
  geom_ribbon(aes(ymin = pmin(pmax(LowerEstCount, 0), max(EstCount)),
                  ymax = pmin(pmax(UpperEstCount, 0), max(EstCount)),
                  fill = "95% confidence interval\nfor estimated total count"),
              alpha = 0.4) +
  scale_fill_manual("Bounds", values = colorPalette[1]) +
  geom_line(aes(y = Count, color = "Reported"), size = 0.2) +
  geom_line(aes(y = EstCount, color = "Estimated total"), size = 0.5) +
  scale_colour_manual("Counts", values = colorPalette[2:3]) +
  ggtitle("Reported and estimated total count of cases") +
  xlab("Diagnosis year") +
  ylab("Count of HIV cases")
print(agregatPlotYr)

# D) STRATIFIED PLOT (OPTIONAL) -------------------------------------------------------------------

if (length(stratVarNames) > 0) {
  # Stratification
  colNames <- union(c("YearOfDiagnosis", "Count", "EstCount", "LowerEstCount", "UpperEstCount"),
                    stratVarNames)
  # Keep only required columns, convert data to "long" format...
  agregatByYearStrat <- melt(agregat[, ..colNames],
                             measure.vars = stratVarNames,
                             variable.name = "Stratum",
                             value.name = "StratumValue")
  # ...and aggregate
  agregatByYearStrat <- agregatByYearStrat[, .(Count = sum(Count),
                                               EstCount = sum(EstCount),
                                               LowerEstCount = sum(LowerEstCount),
                                               UpperEstCount = sum(UpperEstCount)),
                                           by = .(YearOfDiagnosis, Stratum, StratumValue)]

  plotList <- lapply(stratVarNames, function(stratum) {
    ggplot(agregatByYearStrat[Stratum == stratum], aes(x = YearOfDiagnosis)) +
      geom_ribbon(aes(ymin = pmin(pmax(LowerEstCount, 0), max(EstCount)),
                      ymax = pmin(pmax(UpperEstCount, 0), max(EstCount)),
                      fill = "95% confidence interval\nfor estimated total count"),
                  alpha = 0.4) +
      scale_fill_manual("Bounds", values = colorPalette[1]) +
      geom_line(aes(y = Count, color = "Reported"), size = 0.2) +
      geom_line(aes(y = EstCount, color = "Estimated total"), size = 0.5) +
      scale_colour_manual("Counts", values = colorPalette[2:3]) +
      facet_wrap(~ StratumValue, ncol = 2) +
      ggtitle(paste("Reported and estimated total count of cases, by", stratum)) +
      xlab("Diagnosis year") +
      ylab("Count of HIV cases")
  })
  PlotMultipleCharts(plotList)
}
