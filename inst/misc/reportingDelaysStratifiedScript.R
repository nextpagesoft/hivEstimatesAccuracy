# Load libraries
library(hivEstimatesAccuracy)
library(data.table)
library(ggplot2)

# Helper functions
GetRDPlotData <- function(data, by) {
  # Number of imputations in the data (at least one - original data)
  nimp <- data[, length(unique(Imputation)) - 1]

  # Aggregate
  data <- data[, .(Count = sum(Count),
                   EstCount = sum(EstCount),
                   EstCountVar = sum(EstCountVar)),
               by = by]

  # Prepare records for reported data
  dataRep <- data[Source == "Reported"]
  dataRep[, Imputation := NULL]

  # Combine estimates obtained for each imputation
  dataImp <- data[Source == "Imputed",
                  .(Count = mean(Count),
                    EstCount = mean(EstCount),
                    EstCountVarWi = mean(EstCountVar),
                    EstCountVarBe = na.zero(var(EstCount))),
                  by = setdiff(by, "Imputation")]
  dataImp[, ":="(
    EstCountVar = EstCountVarWi + (1 + 1/nimp) * EstCountVarBe,
    EstCountVarWi = NULL,
    EstCountVarBe = NULL
  )]

  # Combine both data sets again
  data <- rbind(dataRep,
                dataImp,
                use.names = TRUE)
  data[, ":="(
    LowerEstCount = EstCount - 1.96 * sqrt(EstCountVar),
    UpperEstCount = EstCount + 1.96 * sqrt(EstCountVar)
  )]

  return(data)
}

GetRDPlots <- function(stratum = NULL, plotData = NULL, isOriginalData = TRUE,
                       colorPalette = c("#AAAAAA", "#E69F00", "#000000", "#56B4E9")) {
  if (!is.null(stratum)) {
    localPlotData <- plotData[Stratum == stratum]
  } else {
    localPlotData <- plotData
  }

  localPlotData <- droplevels(localPlotData)

  localConfBoundsPlotData <- localPlotData[Source == ifelse(isOriginalData,
                                                            "Reported",
                                                            "Imputed")]
  plot <-
    ggplot(data = localPlotData, aes(x = DateOfDiagnosisYear)) +
    geom_ribbon(data = localConfBoundsPlotData,
                aes(ymin = pmin(pmax(LowerEstCount, 0), max(EstCount)),
                    ymax = pmin(pmax(UpperEstCount, 0), max(EstCount)),
                    fill = "95% confidence interval\nfor estimated total count"),
                alpha = 0.4) +
    scale_fill_manual("Bounds", values = colorPalette[1]) +
    geom_line(data = localConfBoundsPlotData,
              aes(x = DateOfDiagnosisYear, y = EstCount, color = "Estimated total"), size = 0.5) +
    geom_line(aes(y = Count, group = Source, color = Source), size = 0.2) +
    scale_colour_manual("Counts", values = colorPalette[2:4]) +
    xlab("Diagnosis year") +
    ylab("Count of HIV cases")

  if (!is.null(stratum)) {
    plot <- plot +
      ggtitle(paste("Reported and estimated total count of cases, by", stratum)) +
      facet_wrap(~ StratumValue, ncol = 2)
  } else {
    plot <- plot +
      ggtitle("Reported and estimated total count of cases")
  }

  return(plot)
}

# Settings

# Separator used for creating a composite of stratum columns. Should not occur in the stratum values.
stratSep <- "_"

# Stratifiation columns
# stratVarNames <- c("Gender", "Transmission", "Migr")
stratVarNames <- c()

# Start year
startYear <- 2000L

# End quarter
endQrt <- 2017.25

# Input data path
inputDataFilePath <- "e:/csv_pilot/ALL.zip"
# inputDataFilePath <- "C:/Users/mrosinska/Documents/projekty/ecdc adjustment/data2017/EL_imp.csv"

# A) READ INPUT DATA ------------------------------------------------------------------------------

# A1 START HERE IF DATA IS BEFORE ANY ADJUSTMENT

# Read input data
originalData <- ReadDataFile(inputDataFilePath)

# Get preliminary attributes mapping
attrMapping <- GetPreliminaryAttributesMapping(originalData)
# Adjust mapping
attrMapping[["FirstCD4Count"]] <- "cd4_num"

# Pre-process data
inputData <- ApplyAttributesMapping(originalData, attrMapping)
inputDataValidityInfo <- ValidateInputData(inputData)
outputData <- PreProcessInputData(inputData)


# outputData <- outputData[ReportingCountry == "AD"]
# # A2 START HERE IF DATA IS ALREADY ADJUSTED WITH MULTIPLE IMPUTATIONS ADJUSTMENT
# outputData <- ReadDataFile(inputDataFilePath)

# B) PROCESS DATA ---------------------------------------------------------------------------------

# Add dummy "Imputation" column if not found
isOriginalData <- !("Imputation" %in% colnames(outputData))
if (isOriginalData) {
  outputData[, Imputation := 0L]
}

# Make sure the strata columns exist in the data
stratVarNames <- stratVarNames[stratVarNames %in% colnames(outputData)]
stratVarNamesImp <- union(c("Imputation", "ReportingCountry"), stratVarNames)

# Create intermediate variables
outputData[, ":="(
  NotificationTime = DateOfNotificationYear + 1/4 * DateOfNotificationQuarter,
  DiagnosisTime = DateOfDiagnosisYear + 1/4 * DateOfDiagnosisQuarter
)]

# Filter
outputData <- outputData[!is.na(DiagnosisTime) & !is.na(NotificationTime)]
outputData <- outputData[DiagnosisTime >= max(startYear + 0.25, min(NotificationTime, na.rm = TRUE))
                         & NotificationTime <= endQrt]
outputData[, VarX := 4 * (NotificationTime - DiagnosisTime)]
outputData[VarX < 0, VarX := NA]
outputData[, ":="(
  MinNotificationTime = min(NotificationTime, na.rm = TRUE),
  MaxNotificationTime = max(NotificationTime, na.rm = TRUE)
), by = .(ReportingCountry)]
outputData[, ":="(
  VarT = 4 * (pmin(MaxNotificationTime, endQrt) - DiagnosisTime) + 1,
  Tf = 4 * (pmin(MaxNotificationTime, endQrt) - pmax(MinNotificationTime, startYear)),
  ReportingDelay = 1L
)]
outputData[, ":="(
  VarXs = Tf - VarX,
  VarTs = Tf - VarT
)]
# NOTE: Otherwise survival model complains
outputData <- outputData[VarXs > VarTs]

totalPlot <- NULL
stratPlotList <- NULL
if (nrow(outputData) > 0) {

  # Create a stratum variable - will be used to merge with the estimations,
  # takes missing categories as separate categories
  outputData[, Stratum := survival::strata(.SD, shortlabel = TRUE, sep = stratSep, na.group = TRUE),
             .SDcols = stratVarNamesImp]

  # Fit a stratified survival model
  model <- outputData[, survival::Surv(time = VarTs,
                                       time2 = VarXs,
                                       event = ReportingDelay)]
  fit <- outputData[, survival::survfit(model ~ Stratum)]
  if (is.null(fit$strata) & length(levels(outputData$Stratum)) == 1) {
    strata <- c(1L)
    names(strata) <- paste0("Stratum=", levels(outputData$Stratum)[1])
  } else {
    strata <- fit$strata
  }

  # Recreating stratum variables to assign them to the delay distribution dataset
  fitStratum <- data.table(
    Delay = fit$time,
    P = fit$surv,
    Var = fit$std.err^2,
    Stratum = factor(rep(seq_along(strata), strata),
                     labels = levels(outputData$Stratum)))
  fitStratum[, (stratVarNamesImp) := tstrsplit(Stratum, stratSep)]
  fitStratum[, VarT := max(Delay) - Delay]
  fitStratum <- fitStratum[VarT >= 0]

  # Aggregate and keep only required dimensions
  agregat <- outputData[, .(Count = .N),
                        by = .(DateOfDiagnosisYear,
                               VarT,
                               Stratum,
                               Source = ifelse(Imputation == 0, "Reported", "Imputed"))]
  agregat <- merge(agregat,
                   fitStratum,
                   on = c("VarT", "Stratum"))

  # Merge fit data
  # agregat <- fitStratum[agregat,
  #                       on = .(VarT, Stratum)]

  # Compute estimated count and its variance
  agregat[, ":="(
    EstCount = Count / P,
    EstCountVar = (Count * (Count + 1) / P^4 * Var) + Count * (1 - P) / P^2
  )]

  # Create final output object
  outputData <- merge(outputData,
                      agregat[, .(VarT, Stratum, Weight = 1 / P)],
                      by = c("VarT", "Stratum"),
                      all.x = TRUE)

  # Keep only columns present in the input object plus the weight
  outColNames <- union(colnames(inputData),
                       c("VarT", "Stratum", "Weight"))
  outputData <- outputData[, ..outColNames]

  # C) TOTAL PLOT ----------------------------------------------------------------------------------
  totalPlotData <- GetRDPlotData(data = agregat,
                                 by = c("Source", "Imputation", "DateOfDiagnosisYear"))
  totalPlot <- GetRDPlots(plotData = totalPlotData,
                          isOriginalData = isOriginalData)

  # D) STRATIFIED PLOT (OPTIONAL) ------------------------------------------------------------------
  if (length(stratVarNames) > 0) {
    # Stratification
    colNames <- union(c("Source", "DateOfDiagnosisYear", "Count", "EstCount", "EstCountVar"),
                      stratVarNamesImp)
    # Keep only required columns, convert data to "long" format...
    agregatLong <- melt(agregat[, ..colNames],
                        measure.vars = stratVarNames,
                        variable.name = "Stratum",
                        value.name = "StratumValue")

    stratPlotData <- GetRDPlotData(data = agregatLong,
                                   by = c("Source", "Imputation", "DateOfDiagnosisYear", "Stratum",
                                          "StratumValue"))
    stratPlotList <- lapply(stratVarNames,
                            GetRDPlots,
                            plotData = stratPlotData,
                            isOriginalData = isOriginalData)
  }
}

# Use arrows in "Plots" tab to the right to browse through the plots
print(totalPlot)
print(stratPlotList)
