list(
  # Adjustment name ----
  Name = "Reporting Delays",

  # Adjustment type ----
  Type = "REPORTING_DELAYS",

  # Adjustment subtype ----
  SubType = "DEFAULT",

  # Input parameters to the adjustment function ----
  Parameters = list(
    # Parameter 1: a specification with label, type and default value
    startYear = list(
      label = "Diagnosis start year",
      value = 2000L,
      input = "numeric"),
    # Parameter 2: a specification with label, type and default value
    endQrt = list(
      label = "Notification end quarter",
      value = 2017.25,
      input = "numeric"),
    stratGender = list(
      name = "stratGender",
      label = "Gender",
      value = FALSE,
      input = "checkbox"),
    stratTrans = list(
      name = "stratTrans",
      label = "Transmission",
      value = FALSE,
      input = "checkbox"),
    stratMigr = list(
      name = "stratMigr",
      label = "Migration",
      value = FALSE,
      input = "checkbox")
  ),

  # Names of packages that must be made available to the adjustment function ----
  RequiredPackageNames = c(),

  ## Adjustment function ----
  AdjustmentFunction = function(inputData, parameters) {

    # Work on a copy
    outputData <- copy(inputData)

    # Separator used for creating a composite of stratum columns. Should not occur in the stratum values.
    stratSep <- "_"

    # Start year
    startYear <- parameters$startYear
    # End quarter
    endQrt <- parameters$endQrt
    # Stratifiation columns
    stratVarNames <- c()
    if (parameters$stratGender) {
      stratVarNames <- union(stratVarNames, "Gender")
    }
    if (parameters$stratTrans) {
      stratVarNames <- union(stratVarNames, "Transmission")
    }
    if (parameters$stratMigr) {
      stratVarNames <- union(stratVarNames, "Migr")
    }
    stratVarNames <- stratVarNames[stratVarNames %in% colnames(outputData)]

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
    outputData <- outputData[DiagnosisTime >= max(startYear + 0.25,
                                                  min(NotificationTime, na.rm = TRUE))
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
    totalPlotData <- NULL
    stratPlotList <- NULL
    stratPlotListData <- NULL
    distribution <- NULL
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

      # Get distribution object as artifact
      varNames <- setdiff(colnames(fitStratum),
                          c("Delay", "P", "Var", "Stratum", "VarT"))
      rdDistribution <- fitStratum[VarT > 0,
                                 union(varNames, c("VarT", "P", "Var")),
                                 with = FALSE]
      setnames(rdDistribution,
               old = "VarT",
               new = "Quarter")
      setorderv(rdDistribution, union(varNames, "Quarter"))

      # Aggregate and keep only required dimensions
      agregat <- outputData[, .(Count = .N),
                            by = .(DateOfDiagnosisYear,
                                   VarT,
                                   Stratum,
                                   Source = ifelse(Imputation == 0, "Reported", "Imputed"))]
      # Merge fit data
      agregat <- fitStratum[agregat,
                            on = .(VarT, Stratum)]

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

        stratPlotListData <- GetRDPlotData(data = agregatLong,
                                           by = c("Source", "Imputation", "DateOfDiagnosisYear",
                                                  "Stratum", "StratumValue"))
        stratPlotList <- lapply(stratVarNames,
                                GetRDPlots,
                                plotData = stratPlotListData,
                                isOriginalData = isOriginalData)

        names(stratPlotList) <- stratVarNames
      }
    }
    artifacts <- list(OutputPlotTotal = totalPlot,
                      OutputPlotTotalData = totalPlotData,
                      OutputPlotStrat = stratPlotList,
                      OutputPlotStratData = stratPlotListData,
                      RdDistribution = rdDistribution)

    cat("No adjustment specific text outputs.\n")

    return(list(Table = outputData,
                Artifacts = artifacts))
  }
)
