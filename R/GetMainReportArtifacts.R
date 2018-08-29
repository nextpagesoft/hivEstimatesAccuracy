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
  colorPalette <- c("#69b023", "#7bbcc0", "#9d8b56", "#ce80ce", "#b23A48",
                    "#7a5980", "#63372c", "#284b63")
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

  # Functions ------------------------------------------------------------------

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
    aggrExpr = ".(Count = .N)"
  ) {
    if (is.null(data)) {
      return(NULL)
    }

    expr <- parse(text = aggrExpr)
    aggr1 <- data[, eval(expr), by = c(rowvar, colvar)]
    aggr2 <- data[, eval(expr), by = c(rowvar)]
    aggr3 <- data[, eval(expr), by = c(colvar)]
    aggr4 <- data[, eval(expr)]

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

    if ("Count_Val" %in% colnames(dt)) {
      dt[,
         Count_Perc := Count_Val / sum(Count_Val, na.rm = TRUE) * 100,
         by = c(rowvar)]
    }

    return(dt)
  }

  GetReportTable <- function(
    data,
    rowvar,
    colvar,
    vvars,
    mapping = colNamesMappingTable,
    digits = 0
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
    if (!is.null(mapping)) {
      mapping <- mapping[names(mapping) %in% colnames(dt)]
      setnames(dt,
               old = names(mapping),
               new = mapping)
    }

    dt <- knitr::kable(dt,
                       align = rep("r", ncol(dt)))

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
    colors = colorPalette
  ) {
    if (is.null(data)) {
      return(NULL)
    }

    filter <- sprintf("DateOfDiagnosisYear != 'Total' & %s != 'Overall'", colvar)
    data <- data[eval(parse(text = filter))]

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
      theme(axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8)) +
      labs(x = "Year",
           y = expression("Median CD4 cell count (cells/"*mu*"L)"))

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
    dt,
    rowvar,
    colvar,
    vvar,
    nsdf,
    probs = c(CD4_Low = 0.25, CD4_Median = 0.5, CD4_High = 0.75)
  ) {
    dataList <- mitools::imputationList(split(
      dt[, c(vvar, colvar, rowvar, "DY", "Imputation", "ModelWeight"),
         with = FALSE],
      by = "Imputation"))
    result <- NULL
    for (probName in names(probs)) {
      prob <- probs[probName]
      if (optSmoothing) {
        models <-
          with(dataList,
               quantreg::rq(formula =
                    as.formula(sprintf("%s ~ %s * splines::ns(DY, df = nsdf)",
                                       vvar, colvar)),
                  tau = prob,
                  data = dataList$imputations,
                  weights = ModelWeight,
                  method = "br"))
        vars <- mitools::MIextract(models, fun = function(model) {
          SparseM::diag(summary(model, covariance = TRUE)$cov)
        })
      } else {
        models <-
          with(dataList,
               quantreg::rqss(formula =
                      as.formula(sprintf("%s ~ %s * as.factor(DY)", vvar, colvar)),
                    tau = prob,
                    data = dataList$imputations,
                    weights = ModelWeight,
                    method = "sfn"))
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
    if (optSmoothing) {
      suppressWarnings({
        models <-
          with(dataList,
               glm(formula =
                     as.formula(sprintf("Count_Val ~ as.factor(%s) * splines::ns(DY, df = nsdf)",
                                        colvar)),
                   family = poisson(link = log)))
      })
    } else {
      suppressWarnings({
        models <-
          with(dataList,
               glm(formula =
                     as.formula(sprintf("Count_Val ~ as.factor(%s) * as.factor(DY)",
                                        colvar)),
                   family = poisson(link = log)))
      })
    }

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
    repeat {
      filteredData <- FilterData(data = data,
                                 colvar = colvar,
                                 badCategories = badCategories)

      result <- suppressWarnings({
        try(modelFunc(colvar = colvar, dt = filteredData, ...),
            silent = TRUE)
      })

      if (!inherits(result, "try-error")) {
        if (length(badCategories) > 0) {
          message <-
            sprintf("<p>Persons which were %s anywhere (i.e. even in one imputed dataset) are removed. Threshold of %2.2f%% of count per category within the dataset was applied.</p>",
                    paste(colNamesMapping[as.character(badCategories)],
                          collapse = ", "),
                    threshold * 100)
        }
        break
      } else {
        badCategories <- tail(categories, 1)
        categories <- setdiff(categories, badCategories)
        result <- NULL
      }
    }

    return(list(Result = result,
                Message = message,
                BadCategories = badCategories))
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
    dt[, (numericCols) := lapply(.SD, FormatNumbers), .SDcols = numericCols]
    setorderv(dt, c("DateOfDiagnosisYear"))
    tableColNames <- c("Diagnosis<br /> year",
                       "Missing<br /> details",
                       "Reported",
                       "Unreported",
                       "Estimated total",
                       "Estimated total<br /> lower bound",
                       "Estimated total<br /> upper bound")
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

  # Original data
  dataOrig <- fullData[Imputation == 0L]
  dataOrig[, ':='(
    CD4 = SqCD4^2,
    Transmission = factor(Transmission),
    Gender = factor(Gender)
  )]

  # MI data
  dataMI <- fullData[Imputation != 0L]
  dataMI[, ':='(
    Transmission = factor(Transmission),
    Gender = factor(Gender)
  )]

  dataMIGenderCountDistr <-
    dataMI[, .(Count = .N), by = .(Gender)][, Perc := Count / sum(Count)]
  dataMITransCountDistr <-
    dataMI[, .(Count = .N), by = .(Transmission)][, Perc := Count / sum(Count)]

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

  dataMIGenderCD4List <- NULL
  dataMITransCD4List <- NULL
  dataMITransCountList <- NULL
  if (miPresent) {
    if (cd4Present) {
      # Quantile regressions not possible with rare categories and discrete time
      # extrapolated - dodgy results from quantile regressions with rare
      # categories and smoothed time rare categories removed.
      dataMIGenderCD4List <-
        GetModelledDataAdaptive(
          data = dataMI,
          modelFunc = GetModelledQuantileData,
          colvar = "Gender",
          rowvar = "DateOfDiagnosisYear",
          vvar = "SqCD4",
          distr = dataMIGenderCountDistr,
          nsdf = nsdf,
          colNamesMapping = colNamesMappingTable)
      dataMITransCD4List <-
        GetModelledDataAdaptive(
          data = dataMI,
          modelFunc = GetModelledQuantileData,
          colvar = "Transmission",
          rowvar = "DateOfDiagnosisYear",
          vvar = "SqCD4",
          distr = dataMITransCountDistr,
          nsdf = nsdf,
          colNamesMapping = colNamesMappingTable)

      transBadCategories <- dataMITransCD4List[["BadCategories"]]
    } else {
      transBadCategories <- c()
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
        colvar = "Transmission",
        distr = dataMITransCountDistr[!Transmission %in% transBadCategories],
        nsdf = nsdf)
    dataMITransCountList[["Result"]] <-
      GetAggregatedData(
        data = dataMITransCountList[["Result"]],
        rowvar = "DateOfDiagnosisYear",
        colvar = "Transmission",
        aggrExpr = "list(Count_Val = sum(Count_Val, na.rm = TRUE))")
  }

  cd4YLim <- NULL
  if (cd4Present) {
    if (miPresent) {
      cd4YLim <-
        GetNiceUpperLimit(max(
          dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
          dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
          dataMIGenderCD4List[["Result"]]$CD4_Median,
          dataMITransCD4List[["Result"]]$CD4_Median,
          na.rm = TRUE))
    } else {
      cd4YLim <-
        GetNiceUpperLimit(max(
          dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
          dataOrigTrans[DateOfDiagnosisYear != "Total", CD4_Median],
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

  # PRODUCE OUTPUTS ------------------------------------------------------------
  tblOrigGenderCount <-
    GetReportTable(data = dataOrigGender,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Gender",
                   vvars = c("Count_Val", "Count_Perc"))
  plotOrigGenderCount <-
    GetReportPlot(data = dataOrigGender,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Gender",
                  vvars = "Count_Val")

  tblOrigGenderCD4 <-
    GetReportTable(data = dataOrigGender,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Gender",
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"))
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
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"))
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
                   vvars = c("Count_Val", "Count_Perc"))
  plotOrigTransCount <-
    GetReportPlot(data = dataOrigTrans,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Transmission",
                  vvars = "Count_Val")
  tblMITransCount <-
    GetReportTable(data = dataMITransCount,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Transmission",
                   vvars = c("Count_Val", "Count_Perc"))
  plotMITransCount <-
    GetReportPlot(data = dataMITransCount,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Transmission",
                  vvars = "Count_Val",
                  cd4YLim = cd4YLim)

  tblOrigTransCD4 <-
    GetReportTable(data = dataOrigTrans,
                   rowvar = "DateOfDiagnosisYear",
                   colvar = "Transmission",
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"))
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
                   vvars = c("CD4_Low", "CD4_Median", "CD4_High"))
  plotMITransCD4 <-
    GetReportPlot(data = dataMITransCD4,
                  rowvar = "DateOfDiagnosisYear",
                  colvar = "Transmission",
                  vvars = c("CD4_Median", "CD4_Low", "CD4_High"),
                  confIntervals = optCD4ConfInt,
                  cd4YLim = cd4YLim)

  tblRd <- GetRDReportTable(data = rdData)

  return(
    list(
      ReportingDelay = optReportingDelay,
      Smoothing = optSmoothing,
      CD4ConfInt = optCD4ConfInt,
      Artifacts = list(
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
        dataMIGenderCD4Message = dataMIGenderCD4List[["Message"]],
        dataMITransCountMessage = dataMITransCountList[["Message"]],
        dataMITransCD4Message = dataMITransCD4List[["Message"]],
        TblRd = tblRd)
      )
    )
}
