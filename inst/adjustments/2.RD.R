list(
  # Adjustment name ----
  Name = "Reporting Delays",

  # Adjustment type ----
  Type = "REPORTING_DELAYS",

  # Adjustment subtype ----
  SubType = "DEFAULT",

  # Input parameters to the adjustment function ----
  Parameters = list(
    startYear = list(
      label = "Diagnosis start year",
      value = 2000L,
      input = "numeric"),
    endYear = list(
      label = "Notification end year",
      value = 2017,
      input = "numeric"),
    endQrt = list(
      label = "Notification end quarter (integer between 1 and 4)",
      value = 1,
      min = 1,
      max = 4,
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

    require(data.table)

    # A) SETUP -------------------------------------------------------------------------------------

    # Work on a copy
    compData <- copy(inputData)

    # Initialize output data
    outputData <- copy(inputData)

    # Separator used for creating a composite of stratum columns. Should not occur in the stratum
    # values.
    stratSep <- "_"

    # Start year
    startYear <- parameters$startYear
    # End quarter
    endQrt <- parameters$endYear + parameters$endQrt / 4
    # Stratifiation columns
    stratVarNames <- c()
    if (parameters$stratGender) {
      stratVarNames <- union(stratVarNames, "Gender")
    }
    if (parameters$stratTrans) {
      stratVarNames <- union(stratVarNames, "Transmission")
    }
    if (parameters$stratMigr) {
      stratVarNames <- union(stratVarNames, "GroupedRegionOfOrigin")
    }
    stratVarNames <- stratVarNames[stratVarNames %in% colnames(compData)]

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
    compData[, Stratum := survival::strata(.SD,
                                           shortlabel = TRUE,
                                           sep = stratSep,
                                           na.group = TRUE),
             .SDcols = stratVarNamesImp]
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

    totalPlot <- NULL
    totalPlotData <- NULL
    stratPlotList <- NULL
    stratPlotListData <- NULL
    rdDistribution <- NULL
    if (nrow(compData) > 0) {
      # Fit a stratified survival model
      model <- compData[, survival::Surv(time = VarTs,
                                         time2 = VarXs,
                                         event = ReportingDelay)]
      fit <- compData[, survival::survfit(model ~ Stratum)]
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
        Var = fit$std.err^2,
        Stratum = factor(rep(seq_along(strata), strata),
                         labels = levels(compData$Stratum)))
      fitStratum[, (stratVarNamesImp) := tstrsplit(Stratum, stratSep)]
      fitStratum[, VarT := max(Delay) - Delay]
      fitStratum <- fitStratum[VarT >= 0]
      # Convert "NA" to NA
      fitStratum[, (stratVarNamesImp) := lapply(.SD, function(x) ifelse(x == "NA", NA_character_, x)),
                 .SDcols = stratVarNamesImp]

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
      agregat[, MissingData := is.na(P)]

      # Create final output object
      outputData <- merge(outputData,
                          agregat[MissingData == FALSE, .(VarT, Stratum, Weight = 1 / P)],
                          by = c("VarT", "Stratum"),
                          all.x = TRUE)

      # C) TOTAL PLOT ----------------------------------------------------------------------------------
      totalPlotData <- GetRDPlotData(data = agregat,
                                     by = c("MissingData", "Source", "Imputation",
                                            "DateOfDiagnosisYear"))
      setorderv(totalPlotData, c("MissingData", "DateOfDiagnosisYear"))
      totalPlot <- GetRDPlots(plotData = totalPlotData[MissingData == FALSE],
                              isOriginalData = isOriginalData)

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
      outputData[, Weight := NA_real_]
    }

    # Keep only columns present in the input object plus the weight
    outColNames <- union(colnames(inputData),
                         c("VarT", "Stratum", "Weight"))
    outputData <- outputData[, ..outColNames]

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
