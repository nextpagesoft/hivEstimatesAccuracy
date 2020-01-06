list(
  # Adjustment name ----
  Name = "Reporting Delays with trend",

  # Adjustment type ----
  Type = "REPORTING_DELAYS",

  # Adjustment subtype ----
  SubType = "TREND",

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
    require(survival)

    # A) SETUP -------------------------------------------------------------------------------------

    # Work on a copy
    compData <- copy(inputData)

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
    stratVarNamesTrend <- union("DateOfDiagnosisYear",
                                stratVarNames)

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
      VarT = 4 * (pmin.int(MaxNotificationTime, endQrt) - DiagnosisTime) + 1,
      Tf = 4 * (pmin.int(MaxNotificationTime, endQrt) - pmax.int(min(DiagnosisTime), startYear + 0.25)) + 1,
      ReportingDelay = 1L
    )]
    compData[, ":="(
      VarXs = Tf - VarX,
      VarTs = Tf - VarT
    )]
    # NOTE: Otherwise survival model complains
    compData <- droplevels(compData[VarXs > VarTs])

    totalPlot <- NULL
    totalPlotData <- NULL
    stratPlotList <- NULL
    stratPlotListData <- NULL
    rdDistribution <- NULL
    reportTableData <- NULL
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
      # RD estimation with time trend

      # Define parameters
      lastYear <- compData[, max(DateOfDiagnosisYear)]
      years <- (lastYear - 4):lastYear
      tGroups <- 1:3
      imputations <- compData[, sort(unique(Imputation))]
      formula <- as.formula(sprintf("Surv(time = VarTs, time2 = VarXs, event = ReportingDelay) ~ (%s):strata(tgroup)",
                                    paste(stratVarNamesTrend, collapse = " + ")))
      cuts <- compData[, c(max(VarXs) - 10,
                           max(VarXs) - 3,
                           max(VarXs))]

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
          VarTs = c(0, cuts)[tgroup],
          VarXs = cuts[tgroup],
          ReportingDelay = 0
        )]

        fit <- try({
          survfit(fitCox,
                  newdata = estFrame,
                  id = Id)
        }, silent = TRUE)

        if (is(fit, "try-error")) {
          fitStratumImp <- data.table(
            Imputation = imputation,
            Delay = 0,
            P = 1,
            Weight = 1,
            Var = 0,
            unique(estFrame[, ..stratVarNamesTrend])
          )
        } else {
          fitStratumImp <- data.table(
            Imputation = imputation,
            Delay = fit$time,
            P = fit$surv,
            Weight = 1/fit$surv,
            Var = fit$std.err^2,
            unique(estFrame[, ..stratVarNamesTrend])[rep(seq_len(.N), fit$strata)])
        }

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
      outputData[, MissingData := is.na(Weight) | is.infinite(Weight)]

      outputData[MissingData == TRUE, ":="(
        Weight = 1,
        P = 1
      )]
      outputData[is.na(Var) | is.infinite(Var), Var := 0]
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

      # C) TOTAL PLOT ----------------------------------------------------------
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

      # D) STRATIFIED PLOT (OPTIONAL) ------------------------------------------
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
    } else {
      outputData[, Weight := 1]
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

    cat("No adjustment specific text outputs.\n")

    return(list(Table = outputData,
                Artifacts = artifacts))
  }
)
