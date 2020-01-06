#' GetMainReportArtifacts
#'
#' Get all tables and plots used for population of the main report.
#'
#' @param params Main report parameters list object
#'
#' @return List of table and plot objects
#'
#' @examples
#' \dontrun{
#' GetMainReportArtifacts(params)
#' }
#'
#' @export
GetMainReportArtifacts <- function(params)
{
  # Functions ------------------------------------------------------------------

  GenerateColors <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    colors <- hcl(h = hues, l = 65, c = 100)[1:n]
    return(colors)
  }


  FormatNumbers <- function(
    x,
    digits = 0
  ) {
    selNA <- is.na(x)
    res <- rep("-", length(x))
    res[!selNA] <- sprintf(paste0("%.", digits, "f"), x[!selNA])

    return(res)
  }

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
    aggrExpr = ".(Count = .N)",
    rowvarSummaryName = "Total",
    colvarSummaryName = "Overall"
  ) {
    if (is.null(data)) {
      return(NULL)
    }

    expr <- parse(text = aggrExpr)
    aggr1 <- data[, eval(expr), by = c(rowvar, colvar)]
    if ("Count_Val" %in% colnames(aggr1)) {
      aggr1[,
            Count_Perc := Count_Val / sum(Count_Val, na.rm = TRUE) * 100,
            by = c(rowvar)]
    }

    aggr2 <- data[, eval(expr), by = c(rowvar)]
    if ("Count_Val" %in% colnames(aggr2)) {
      aggr2[, Count_Perc := 100]
    }

    aggr3 <- data[, eval(expr), by = c(colvar)]
    if ("Count_Val" %in% colnames(aggr3)) {
      aggr3[, Count_Perc := Count_Val / sum(Count_Val, na.rm = TRUE) * 100]
    }

    aggr4 <- data[, eval(expr)]
    if ("Count_Val" %in% colnames(aggr4)) {
      aggr4[, Count_Perc := 100]
    }

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

  GetReportTable <- function(
    data,
    rowvar,
    colvar,
    vvars,
    mapping = colNamesMappingN,
    digits = 0,
    overallColName = "Overall",
    totalRowName = "Total"
  ) {
    if (is.null(data)) {
      return(NULL)
    }

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
    if ("Overall" %in% colnames(dt)) {
      setnames(dt, old = "Overall", new = overallColName)
    }
    dt[get(rowvar) == "Total", (rowvar) := totalRowName]
    if (!is.null(mapping)) {
      mapping <- mapping[names(mapping) %in% colnames(dt)]
      setnames(dt,
               old = names(mapping),
               new = mapping)
    }

    dt <- knitr::kable(dt,
                       align = rep("r", ncol(dt)),
                       table.attr = "style={width: auto}")

    return(dt)
  }

  GetReportPlot <- function(
    data,
    rowvar,
    colvar,
    vvars,
    cd4YLim = NULL,
    probsStr = NULL,
    confIntervals = FALSE,
    mapping = colNamesMapping,
    colors = colorPalette,
    yLabel = expression("Median CD4 cell count (cells/"*mu*"L)")
  ) {
    if (is.null(data)) {
      return(NULL)
    }

    filter <- sprintf("DateOfDiagnosisYear != 'Total' & %s != 'Overall'", colvar)
    data <- data[eval(parse(text = filter))]

    n <- data[, length(unique(get(colvar)))]
    if (n > length(colors)) {
      extraColors <- GenerateColors(n - length(colors))
      colors <- c(colors, extraColors)
    }

    plotObj <- ggplot(data = data,
                      aes(x = as.integer(get(rowvar)),
                          y = get(vvars[1]),
                          color = get(colvar),
                          fill = get(colvar))) +
      geom_line(size = 0.5) +
      geom_point(size = 1.5) +
      scale_x_continuous(expand = c(0, 0),
                         breaks = data[, as.integer(sort(unique(get(rowvar))))]) +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(y = c(0, cd4YLim)) +
      scale_colour_manual(name = colvar,
                          labels = mapping,
                          values = colors) +
      scale_fill_manual(name = colvar,
                        labels = mapping,
                        values = colors) +
      theme_classic() +
      theme(plot.title = element_text(size = 9, face = "plain"),
            text = element_text(size = 9, face = "plain"),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7)) +
      labs(x = "Year",
           y = yLabel)

    if (confIntervals) {
      plotObj <- plotObj +
        geom_ribbon(aes(ymin = get(vvars[2]),
                        ymax = get(vvars[3])),
                    alpha = 0.1,
                    colour = NA)
    }

    plotObj <- RecordGgplot(plotObj)

    return(plotObj)
  }

  GetModelledQuantileData <- function(
    dt,
    rowvar,
    colvar,
    vvar,
    nsdf,
    probs = c(CD4_Low = 0.25, CD4_Median = 0.5, CD4_High = 0.75)
  ) {
    dataList <- mitools::imputationList(split(
      dt[, c(vvar, colvar, rowvar, "DY", "Imputation", "ModelWeight"), with = FALSE],
      by = "Imputation"
    ))

    if (dt[, length(unique(get(colvar))) > 1]) {
      colVar1 <- colvar
    } else {
      colVar1 <- "1"
    }
    colVar2 <- ""

    result <- NULL
    for (probName in names(probs)) {
      prob <- probs[probName]
      if (optSmoothing) {
        if (dt[, length(unique(DY)) > 1]) {
          colVar2 <- "* splines::ns(DY, df = nsdf)"
        }
        models <-
          with(
            dataList,
            quantreg::rq(
              formula = as.formula(sprintf("%s ~ %s %s", vvar, colVar1, colVar2)),
              tau = prob,
              data = dataList$imputations,
              weights = ModelWeight,
              method = "br"
            )
          )
        vars <- mitools::MIextract(models, fun = function(model) {
          SparseM::diag(summary(model, covariance = TRUE)$cov)
        })
      } else {
        if (dt[, length(unique(DY)) > 1]) {
          colVar2 <- "* as.factor(DY)"
        }
        models <-
          with(
            dataList,
            quantreg::rqss(
              formula = as.formula(sprintf("%s ~ %s %s", vvar, colVar1, colVar2)),
              tau = prob,
              data = dataList$imputations,
              weights = ModelWeight,
              method = "sfn"
            )
          )
        vars <- mitools::MIextract(models, fun = function(model) {
          SparseM::diag(SparseM::as.matrix(summary(model, cov = TRUE)$Vcov))
        })
      }

      betas <- mitools::MIextract(models, fun = coefficients)
      t <- mitools::MIcombine(betas, vars)
      X <- SparseM::model.matrix(models$`1`$formula)
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

  GetModelledCountData <- function(
    dt,
    colvar,
    nsdf
  ) {
    dt <- copy(dt)
    dt[is.infinite(ModelWeight), ModelWeight := 1]

    dt <- dt[,
             .(Count_Val = sum(ModelWeight, na.rm = TRUE)),
             by = c("Imputation", "DateOfDiagnosisYear", colvar)]
    if (nrow(dt) > 0) {
      dt[, DY := DateOfDiagnosisYear - min(DateOfDiagnosisYear)]
    } else {
      dt[, DY := integer()]
    }

    # mitools doesn't like factors with 0 frequency levels
    dt[, (colvar) := droplevels(get(colvar))]

    # Fit saturated Poisson model to MI data
    dataList <- mitools::imputationList(split(dt, by = "Imputation"))

    # Main model
    colVar2 <- ""
    if (dt[, length(unique(DY)) > 1]) {
      if (optSmoothing) {
        colVar2 <- "* splines::ns(DY, df = nsdf)"
      } else {
        colVar2 <- "* as.factor(DY)"
      }
    }

    suppressWarnings({
      models <-
        with(
          dataList,
          glm(
            formula = as.formula(sprintf("Count_Val ~ as.factor(%s) %s", colvar, colVar2)),
            family = poisson(link = log)
          )
        )
    })

    # Extract betas and var
    betas <- mitools::MIextract(models, fun = coefficients)
    vars <- mitools::MIextract(models, fun = vcov)

    # Rubin's rules applied by MIcombine
    t <- mitools::MIcombine(results = betas, variances = vars)
    X <- SparseM::model.matrix(models$`1`$formula)
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
                  Count_Val = as.vector(linpred))
    pred <- pred[, c("DateOfDiagnosisYear", colvar, "Count_Val"), with = FALSE]

    return(pred)
  }

  GetModelledDataAdaptive <- function(
    data,
    colvar,
    colNamesMapping,
    distr,
    modelFunc,
    ...
  ) {
    result <- NULL
    message <- NULL
    badCategories <- c()
    categories <- distr[order(-Perc), get(colvar)]
    iter <- 0
    repeat {
      iter <- iter + 1

      filteredData <- FilterData(
        data = data,
        colvar = colvar,
        badCategories = badCategories
      )

      if (nrow(filteredData) == 0) {
        return(list(
          Result = NULL,
          Message = sprintf(
            "<p>No records left after removing persons which were %s anywhere (i.e. even in one imputed dataset).</p>",
            paste(colNamesMapping[as.character(badCategories)], collapse = ", ")
          ),
          BadCategories = badCategories
        ))
      }

      if (iter > 10) {
        return(list(
          Result = NULL,
          Message = "<p>No results reached after 10 iterations of adaptive modelling algorithm.</p>",
          BadCategories = badCategories
        ))
      }

      result <- suppressWarnings({
        try(modelFunc(colvar = colvar, dt = filteredData, ...),
            silent = TRUE)
      })

      if (!inherits(result, "try-error")) {
        if (length(badCategories) > 0) {
          message <-
            sprintf("<p>Persons which were %s anywhere (i.e. even in one imputed dataset) are removed.</p>",
                    paste(colNamesMapping[as.character(badCategories)], collapse = ", "))
        }
        break
      } else {
        badCategories <- union(badCategories, tail(categories, 1))
        categories <- setdiff(categories, badCategories)
        result <- NULL
      }
    }

    return(list(
      Result = result,
      Message = message,
      BadCategories = badCategories
    ))
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

  GetRDReportTable <- function(data)
  {
    if (is.null(data)) {
      return(NULL)
    }

    dt <- copy(data)

    numericCols <- setdiff(colnames(dt), c("DateOfDiagnosisYear"))
    dtTotals <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = numericCols]
    dtTotals[, DateOfDiagnosisYear := "Total"]
    ConvertDataTableColumns(dt, c(DateOfDiagnosisYear = "character"))
    dt <- rbind(dt,
                dtTotals)
    singleValCols <- c("Reported", "RDWeightEstimated", "RDWeightNotEstimated")
    dt[, (singleValCols) := lapply(.SD, FormatNumbers), .SDcols = singleValCols]
    dt[, EstUnreported := FormatRangeCols(.SD), .SDcols = c("LowerEstUnreported", "EstUnreported", "UpperEstUnreported")]
    dt[, EstCount := FormatRangeCols(.SD), .SDcols = c("LowerEstCount", "EstCount", "UpperEstCount")]
    dt[, ":="(
      LowerEstCount = NULL,
      UpperEstCount = NULL,
      LowerEstUnreported = NULL,
      UpperEstUnreported = NULL
    )]
    setorderv(dt, c("DateOfDiagnosisYear"))
    tableColNames <- c("Diagnosis<br /> year",
                       "Reported<br /> &nbsp;",
                       "Weight<br /> estimated",
                       "Weight<br /> not estimated",
                       "Estimated<br /> unreported<br /> [N (95% CI)]",
                       "Estimated<br /> total<br /> [N (95% CI)]")
    dt <- knitr::kable(dt,
                       align = rep("r", ncol(dt)),
                       col.names = tableColNames)

    return(dt)
  }

  optReportingDelay <- as.logical(params$ReportingDelay)
  optSmoothing <- as.logical(params$Smoothing)
  optCD4ConfInt <- as.logical(params$CD4ConfInt)

  finalDataIdx <- length(params$AdjustedData)
  fullData <- copy(params$AdjustedData[[finalDataIdx]]$Table)

  cd4Present <- fullData[, any(!is.na(SqCD4))]
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
    fullData[, Imputation := 0L]
  }

  if (rdPresent && optReportingDelay) {
    fullData[, ModelWeight := Weight]
  } else {
    fullData[, ModelWeight := 1.0]
  }

  # A. Make manipulations ---
  fullData[Transmission %in% c(NA, "NA", ""),
           Transmission := "Missing"]

  fullData[GroupedRegionOfOrigin %in% c(NA, "NA", ""),
           GroupedRegionOfOrigin := "Missing"]

  colorPalette <- c("#69b023", "#7bbcc0", "#9d8b56", "#ce80ce", "#b23A48",
                    "#7a5980", "#63372c", "#284b63")

  migrVals <- setdiff(fullData[, levels(GroupedRegionOfOrigin)], "Missing")
  names(migrVals) <- migrVals

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
      Missing = "Missing",
      migrVals)
  colNamesMappingN <-
    setNames(
      c(colNamesMapping[1],
        paste(colNamesMapping[-1], "[N (%)]")),
      names(colNamesMapping))
  colNamesMappingCD4 <-
    setNames(
      c(colNamesMapping[1],
        paste(colNamesMapping[-1], "[Median (IQR)]")),
      names(colNamesMapping))

  # Original data
  dataOrig <- fullData[Imputation == 0L]
  dataOrig[, ':='(
    CD4 = SqCD4^2,
    Transmission = factor(Transmission),
    Gender = factor(Gender),
    Migration = factor(GroupedRegionOfOrigin)
  )]

  # MI data
  dataMI <- fullData[Imputation != 0L]
  dataMI[, ':='(
    Transmission = factor(Transmission),
    Gender = factor(Gender),
    Migration = factor(GroupedRegionOfOrigin)
  )]

  dataMIGenderCountDistr <-
    dataMI[, .(Count = .N), by = .(Gender)][, Perc := Count / sum(Count)]
  dataMITransCountDistr <-
    dataMI[, .(Count = .N), by = .(Transmission)][, Perc := Count / sum(Count)]
  dataMIMigrCountDistr <-
    dataMI[, .(Count = .N), by = .(Migration)][, Perc := Count / sum(Count)]

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

  dataOrigMigr <-
    GetAggregatedData(
      data = dataOrig,
      rowvar = "DateOfDiagnosisYear",
      colvar = "Migration",
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

  dataMIGenderCD4List <- NULL
  dataMITransCD4List <- NULL
  dataMITransCountList <- NULL
  dataMIMigrCD4List <- NULL
  dataMIMigrCountList <- NULL
  if (miPresent) {
    if (cd4Present) {
      # Quantile regressions not possible with rare categories and discrete time
      # extrapolated - dodgy results from quantile regressions with rare
      # categories and smoothed time rare categories removed.
      dataMIGenderCD4List <-
        GetModelledDataAdaptive(
          data = dataMI,
          modelFunc = GetModelledQuantileData,
          colNamesMapping = colNamesMapping,
          colvar = "Gender",
          rowvar = "DateOfDiagnosisYear",
          vvar = "SqCD4",
          distr = dataMIGenderCountDistr,
          nsdf = nsdf)
      dataMITransCD4List <-
        GetModelledDataAdaptive(
          data = dataMI,
          modelFunc = GetModelledQuantileData,
          colNamesMapping = colNamesMapping,
          colvar = "Transmission",
          rowvar = "DateOfDiagnosisYear",
          vvar = "SqCD4",
          distr = dataMITransCountDistr,
          nsdf = nsdf)
      dataMIMigrCD4List <-
        GetModelledDataAdaptive(
          data = dataMI,
          modelFunc = GetModelledQuantileData,
          colNamesMapping = colNamesMapping,
          colvar = "Migration",
          rowvar = "DateOfDiagnosisYear",
          vvar = "SqCD4",
          distr = dataMIMigrCountDistr,
          nsdf = nsdf)

      transBadCategories <- dataMITransCD4List[["BadCategories"]]
      migrBadCategories <- dataMIMigrCD4List[["BadCategories"]]
    } else {
      transBadCategories <- NULL
      migrBadCategories <- NULL
    }

    # Prefilter data on the same categories as in CD4 modelling
    dataMI <-
      FilterData(
        data = dataMI,
        colvar = "Transmission",
        badCategories = transBadCategories)
    dataMITransCountList <-
      GetModelledDataAdaptive(
        data = dataMI,
        modelFunc = GetModelledCountData,
        colNamesMapping = colNamesMapping,
        colvar = "Transmission",
        distr = dataMITransCountDistr[!Transmission %in% transBadCategories],
        nsdf = nsdf)
    dataMITransCountList[["Result"]] <-
      GetAggregatedData(
        data = dataMITransCountList[["Result"]],
        rowvar = "DateOfDiagnosisYear",
        colvar = "Transmission",
        aggrExpr = "list(Count_Val = sum(Count_Val, na.rm = TRUE))")
    dataMIMigrCountList <-
      GetModelledDataAdaptive(
        data = dataMI,
        modelFunc = GetModelledCountData,
        colNamesMapping = colNamesMapping,
        colvar = "Migration",
        distr = dataMIMigrCountDistr[!Migration %in% migrBadCategories],
        nsdf = nsdf)
    dataMIMigrCountList[["Result"]] <-
      GetAggregatedData(
        data = dataMIMigrCountList[["Result"]],
        rowvar = "DateOfDiagnosisYear",
        colvar = "Migration",
        aggrExpr = "list(Count_Val = sum(Count_Val, na.rm = TRUE))")
  }

  cd4YLim <- NULL
  if (cd4Present) {
    if (miPresent) {
      cd4YLim <-
        GetNiceUpperLimit(max(
          dataOrigGender[DateOfDiagnosisYear != "Total", CD4_Median],
          dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
          dataOrigMigr[DateOfDiagnosisYear != "Total", CD4_Median],
          dataMIGenderCD4List[["Result"]]$CD4_Median,
          dataMITransCD4List[["Result"]]$CD4_Median,
          dataMIMigrCD4List[["Result"]]$CD4_Median,
          na.rm = TRUE))
    } else {
      cd4YLim <-
        GetNiceUpperLimit(max(
          dataOrigGender[DateOfDiagnosisYear != "Total", CD4_Median],
          dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
          dataOrigMigr[DateOfDiagnosisYear != "Total", CD4_Median],
          na.rm = TRUE))
    }
  }

  rdData <- NULL
  if (rdPresent) {
    rdIdx <- tail(grep("^REPORTING_DELAYS$", adjTypes), 1)
    rdData <- params[["AdjustedData"]][[rdIdx]][["Artifacts"]][["ReportTableData"]]
  }

  dataMIGenderCD4 <- dataMIGenderCD4List[["Result"]]
  dataMITransCD4 <- dataMITransCD4List[["Result"]]
  dataMITransCount <- dataMITransCountList[["Result"]]
  dataMIMigrCD4 <- dataMIMigrCD4List[["Result"]]
  dataMIMigrCount <- dataMIMigrCountList[["Result"]]

  # PRODUCE OUTPUTS ------------------------------------------------------------
  tblOrigGenderCount <-
    GetReportTable(data = dataOrigGender,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Gender",
                   vvars = c("Count_Val", "Count_Perc"),
                   overallColName = "Total",
                   mapping = colNamesMappingN)
  plotOrigGenderCount <-
    GetReportPlot(data = dataOrigGender,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Gender",
                  vvars = "Count_Val",
                  confIntervals = FALSE,
                  yLabel = "Number of cases")
  tblOrigGenderCD4 <-
    GetReportTable(data = dataOrigGender,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Gender",
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"),
                   totalRowName = "Overall",
                   mapping = colNamesMappingCD4)
  plotOrigGenderCD4 <-
    GetReportPlot(data = dataOrigGender,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Gender",
                  vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                  confIntervals = optCD4ConfInt,
                  cd4YLim = cd4YLim)
  tblMIGenderCD4 <-
    GetReportTable(data = dataMIGenderCD4,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Gender",
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"),
                   mapping = colNamesMappingCD4)
  plotMIGenderCD4 <-
    GetReportPlot(data = dataMIGenderCD4,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Gender",
                  vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                  confIntervals = optCD4ConfInt,
                  cd4YLim = cd4YLim)


  tblOrigTransCount <-
    GetReportTable(data = dataOrigTrans,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Transmission",
                   vvars = c("Count_Val", "Count_Perc"),
                   totalRowName = "Overall",
                   mapping = colNamesMappingN)
  plotOrigTransCount <-
    GetReportPlot(data = dataOrigTrans,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Transmission",
                  vvars = "Count_Val",
                  confIntervals = FALSE,
                  yLabel = "Number of cases")
  tblMITransCount <-
    GetReportTable(data = dataMITransCount,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Transmission",
                   vvars = c("Count_Val", "Count_Perc"),
                   totalRowName = "Overall",
                   mapping = colNamesMappingN)
  plotMITransCount <-
    GetReportPlot(data = dataMITransCount,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Transmission",
                  vvars = "Count_Val",
                  confIntervals = FALSE,
                  yLabel = "Number of cases")
  tblOrigTransCD4 <-
    GetReportTable(data = dataOrigTrans,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Transmission",
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"),
                   totalRowName = "Overall",
                   mapping = colNamesMappingCD4)
  plotOrigTransCD4 <-
    GetReportPlot(data = dataOrigTrans,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Transmission",
                  vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                  confIntervals = optCD4ConfInt,
                  cd4YLim = cd4YLim)
  tblMITransCD4 <-
    GetReportTable(data = dataMITransCD4,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Transmission",
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"),
                   mapping = colNamesMappingCD4)
  plotMITransCD4 <-
    GetReportPlot(data = dataMITransCD4,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Transmission",
                  vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                  confIntervals = optCD4ConfInt,
                  cd4YLim = cd4YLim)


  tblOrigMigrCount <-
    GetReportTable(data = dataOrigMigr,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Migration",
                   vvars = c("Count_Val", "Count_Perc"),
                   totalRowName = "Overall",
                   mapping = colNamesMappingN)
  plotOrigMigrCount <-
    GetReportPlot(data = dataOrigMigr,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Migration",
                  vvars = "Count_Val",
                  confIntervals = FALSE,
                  yLabel = "Number of cases")
  tblMIMigrCount <-
    GetReportTable(data = dataMIMigrCount,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Migration",
                   vvars = c("Count_Val", "Count_Perc"),
                   totalRowName = "Overall",
                   mapping = colNamesMappingN)
  plotMIMigrCount <-
    GetReportPlot(data = dataMIMigrCount,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Migration",
                  vvars = "Count_Val",
                  confIntervals = FALSE,
                  yLabel = "Number of cases")
  tblOrigMigrCD4 <-
    GetReportTable(data = dataOrigMigr,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Migration",
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"),
                   totalRowName = "Overall",
                   mapping = colNamesMappingCD4)
  plotOrigMigrCD4 <-
    GetReportPlot(data = dataOrigMigr,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Migration",
                  vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                  confIntervals = optCD4ConfInt,
                  cd4YLim = cd4YLim)
  tblMIMigrCD4 <-
    GetReportTable(data = dataMIMigrCD4,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Migration",
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"),
                   mapping = colNamesMappingCD4)
  plotMIMigrCD4 <-
    GetReportPlot(data = dataMIMigrCD4,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Migration",
                  vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                  confIntervals = optCD4ConfInt,
                  cd4YLim = cd4YLim)


  tblRd <- GetRDReportTable(data = rdData)

  fileNames <- GetAdjustmentSpecFileNames()
  adjustments <- lapply(params$AdjustedData, function(dt) {
    fileName <- fileNames[dt$Name]
    labels <- sapply(GetListObject(fileName, section = "Parameters"), "[[", "label")
    list(
      Name = dt$Name,
      Parameters = lapply(names(dt$Parameters), function(paramName) {
        list(Value = dt$Parameters[[paramName]],
             Label = labels[[paramName]])
      })
    )
  })

  return(
    list(
      ReportingDelay = optReportingDelay,
      Smoothing = optSmoothing,
      CD4ConfInt = optCD4ConfInt,
      Artifacts = list(
        Adjustments = adjustments,
        MIPresent = miPresent,
        RDPresent = rdPresent,
        CD4Present = cd4Present,
        TblOrigGenderCount = tblOrigGenderCount,
        PlotOrigGenderCount = plotOrigGenderCount,
        TblOrigGenderCD4 = tblOrigGenderCD4,
        PlotOrigGenderCD4 = plotOrigGenderCD4,
        TblMIGenderCD4 = tblMIGenderCD4,
        PlotMIGenderCD4 = plotMIGenderCD4,
        TblOrigTransCount = tblOrigTransCount,
        PlotOrigTransCount = plotOrigTransCount,
        TblMITransCount = tblMITransCount,
        PlotMITransCount = plotMITransCount,
        TblOrigTransCD4 = tblOrigTransCD4,
        PlotOrigTransCD4 = plotOrigTransCD4,
        TblMITransCD4 = tblMITransCD4,
        PlotMITransCD4 = plotMITransCD4,
        TblOrigMigrCount = tblOrigMigrCount,
        PlotOrigMigrCount = plotOrigMigrCount,
        TblMIMigrCount = tblMIMigrCount,
        PlotMIMigrCount = plotMIMigrCount,
        TblOrigMigrCD4 = tblOrigMigrCD4,
        PlotOrigMigrCD4 = plotOrigMigrCD4,
        TblMIMigrCD4 = tblMIMigrCD4,
        PlotMIMigrCD4 = plotMIMigrCD4,
        DataMIGenderCD4Message = dataMIGenderCD4List[["Message"]],
        DataMITransCountMessage = dataMITransCountList[["Message"]],
        DataMITransCD4Message = dataMITransCD4List[["Message"]],
        DataMIMigrCountMessage = dataMIMigrCountList[["Message"]],
        DataMIMigrCD4Message = dataMIMigrCD4List[["Message"]],
        TblRd = tblRd)
      )
    )
}
