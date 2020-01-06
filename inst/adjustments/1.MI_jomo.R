list(
  # Adjustment name ----
  Name = "Joint Modelling Multiple Imputation",

  # Adjustment type ----
  Type = "MULTIPLE_IMPUTATIONS",

  # Adjustment subtype ----
  SubType = "JOMO",

  # Input parameters to the adjustment function ----
  Parameters = list(
    # Parameter 1: a specification with label, type and default value
    nimp = list(
      label = "Number of imputations",
      value = 2L,
      input = "numeric"),
    # Parameter 2
    nburn = list(
      label = "Number of burn-in iterations",
      value = 100L,
      input = "numeric"),
    # Parameter 3
    nbetween = list(
      label = "Number of iterations between two successive imputations",
      value = 100L,
      input = "numeric"),
    # Parameter 4
    nsdf = list(
      label = "Number of degrees of freedom for spline of diagnosis calendar year",
      value = 4L,
      min = 3L,
      max = 5L,
      step = 1L,
      ticks = TRUE,
      round = TRUE,
      input = "slider"),
    # Parameter 5
    imputeRD = list(
      label = "Impute reporting delays inputs",
      value = FALSE,
      input = "checkbox")
    # Parameter 6
    # runInParallel = list(
    #   label = "Run in parallel",
    #   value = FALSE,
    #   input = "checkbox")
  ),

  # Names of packages that must be made available to the adjustment function ----
  RequiredPackageNames = c("jomo"),

  ## Adjustment function ----
  AdjustmentFunction = function(inputData, parameters) {

    require(data.table)

    # Perform imputations per data set.
    # This is the actual worker function.
    workerFunction <- function(i, nburn, nbetween, nimp, nsdf, imputeRD) {

      cat("\n")
      cat(sprintf("Processing gender: %s\n", names(dataSets)[i]))

      dataSet <- dataSets[[i]]

      artifacts <- list()

      # Define covariates
      xColNamesAll <- c("AIDS")
      # Define outcomes
      yColNamesAll <- c("Age", "SqCD4", "Transmission", "GroupedRegionOfOrigin")

      if (imputeRD) {
        # Create logit transform of VarX
        dataSet[, c("LogitVarX", "LogTweakedMaxPossibleDelay") := {
          p <- TweakedVarX / TweakedMaxPossibleDelay
          logitVarX <- log(p / (1 - p))
          logTweakedMaxPossibleDelay <- log(TweakedMaxPossibleDelay)
          list(logitVarX, logTweakedMaxPossibleDelay)
        }]

        xColNamesAll <- union(xColNamesAll, c("LogTweakedMaxPossibleDelay"))
        yColNamesAll <- union(yColNamesAll, c("LogitVarX"))
      }

      # Determine which columns to pass to the jomo package

      # At least 2 distinct values present
      xFilterFunc <- function(colName) length(unique(dataSet[[colName]])) >= 2
      # At least one non-NA
      yFilterFunc <- function(colName) !all(is.na(dataSet[[colName]]))

      # Keep only column names meeting requirement
      xColNames <- Filter(xFilterFunc, xColNamesAll)
      yColNames <- Filter(yFilterFunc, yColNamesAll)

      # Keep for reporting
      artifacts[["X_COLS"]] <- list(All = xColNamesAll,
                                    Kept = xColNames)
      artifacts[["Y_COLS"]] <- list(All = yColNamesAll,
                                    Kept = yColNames)

      # Create splines with proper names and intercept
      splineBasisMatrix <- try(as.data.table(splines::ns(dataSet$DY, df = nsdf)), silent = TRUE)
      if (inherits(splineBasisMatrix, "try-error")) {
        splineBasisMatrix <- data.table()
      } else {
        setnames(splineBasisMatrix, paste0("SplineKnot.", colnames(splineBasisMatrix)))
      }

      intercept <- 1L

      # Define covariates of joint imputation model
      X <- cbind(Intercept = intercept,
                 splineBasisMatrix)
      if (length(xColNames) > 0) {
        X <- cbind(dataSet[, ..xColNames],
                   X)
      }

      if (length(yColNames) > 0) {
        # Define outcomes of joint imputation model
        Y <- dataSet[, ..yColNames]

        # Workaround for jomo bug:
        # Not used levels must be removed
        X <- droplevels(X)
        Y <- droplevels(Y)

        # Run model
        cat("Running MCMC sampler.\n\n")
        mcmc <- jomo::jomo.MCMCchain(Y = Y, X = X, nburn = nburn, output = 0)

        artifacts[["Beta"]] <- mcmc$collectbeta
        artifacts[["Covariance"]] <- mcmc$collectomega

        cat("\nPerforming imputation.\n")
        cat("Number of burn-in iterations set to 10 for the actual imputation.\n\n")
        imp <- setDT(jomo::jomo(Y = Y,
                                X = X,
                                nburn = 10,
                                nbetween = nbetween,
                                nimp = nimp,
                                beta.start = mcmc$collectbeta[, , nburn],
                                l1cov.start = mcmc$collectomega[, , nburn],
                                out.iter = 100))
      } else {
        imp <- data.table(Imputation = 0L,
                          id = seq_len(nrow(Y)))
      }

      indexColNames <- c("Imputation", "id")
      impColNames <- union(indexColNames, yColNames)
      dataSetColNames <- setdiff(colnames(dataSet),
                                 union(yColNames, "LogTweakedMaxPossibleDelay"))

      mi <- cbind(imp[, ..impColNames],
                  dataSet[, ..dataSetColNames])

      # Convert LogitVarX back to VarX
      if ("LogitVarX" %in% colnames(mi)) {
        mi[Imputation != 0 & is.na(VarX),
           VarX := MaxPossibleDelay * round(exp(LogitVarX)/(1 + LogitVarX))]
      }

      setcolorder(mi, union(indexColNames, dataSetColNames))

      ConvertDataTableColumns(mi, c(Imputation = "integer"))

      return(list(Table = mi,
                  Artifacts = artifacts))
    }

    # 1. Save original order for later
    inputData[, OrigSort := .I]

    # 2. Measures years from earlier diagnosis year
    inputData[, DY := DateOfDiagnosisYear - min(DateOfDiagnosisYear, na.rm = TRUE)]

    # 3. Split by gender to data sets
    dataSets <- inputData[Gender != ""]
    dataSets <- split(dataSets, by = c("Gender"))

    # 4. Execute the worker function per data set
#     if (parameters$runInParallel) {
#       # Run in parallel
#       libPaths <- .libPaths()
#       cl <- parallel::makeCluster(2)
#       parallel::clusterExport(cl, c("libPaths", "ConvertDataTableColumns", "dataSets"))
#       parallel::clusterEvalQ(cl, {
# 				.libPaths(libPaths)
#         library(data.table)
#       })
#       outputData <- parallel::parLapply(cl,
#                                         seq_along(dataSets),
#                                         workerFunction,
#                                         nburn = parameters$nburn,
#                                         nbetween = parameters$nbetween,
#                                         nimp = parameters$nimp,
#                                         nsdf = parameters$nsdf)
#       parallel::stopCluster(cl)
#     } else {
      # Run sequentially
      outputData <- lapply(seq_along(dataSets),
                           workerFunction,
                           nburn = parameters$nburn,
                           nbetween = parameters$nbetween,
                           nimp = parameters$nimp,
                           nsdf = parameters$nsdf,
                           imputeRD = parameters$imputeRD)
    # }

    # 5. Combine all data sets
    names(outputData) <- names(dataSets)
    table <- rbindlist(lapply(outputData, "[[", "Table"))
    artifacts <- lapply(outputData, "[[", "Artifacts")

    # 6. Restore original order per Imputation
    setorder(table, Imputation, OrigSort)

    return(
      list(Table = table,
           Artifacts = artifacts)
    )
  }
)
