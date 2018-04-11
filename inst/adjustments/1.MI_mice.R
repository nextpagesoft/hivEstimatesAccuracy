list(
  # Adjustment name ----
  Name = "Multiple Imputations (mice)",

  # Adjustment type ----
  Type = "MULTIPLE_IMPUTATIONS",

  # Adjustment subtype ----
  SubType = "MICE",

  # Input parameters to the adjustment function ----
  Parameters = list(
    # Parameter 1: a specification with label, type and default value
    nimp = list(
      label = "Number of imputations",
      value = 2L,
      input = "numeric"),
    # Parameter 2
    nit = list(
      label = "Number of mice iterations",
      value = 10L,
      input = "numeric"),
    # Parameter 3
    nsdf = list(
      label = "Number of degrees of freedom for spline",
      value = 5L,
      min = 3L,
      max = 5L,
      step = 1L,
      ticks = TRUE,
      round = TRUE,
      input = "slider"),
    # Parameter 5
    runInParallel = list(
      label = "Run in parallel",
      value = FALSE,
      input = "checkbox")
  ),

  # Names of packages that must be made available to the adjustment function ----
  RequiredPackageNames = c("mice"),

  ## Adjustment function ----
  AdjustmentFunction = function(inputData, parameters) {

    # Perform imputations per data set.
    # This is the actual worker function.
    workerFunction <- function(i, nit, nimp, nsdf) {

      cat("\n")
      cat(sprintf("Processing gender: %s\n", names(dataSets)[i]))

      dataSet <- dataSets[[i]]

      artifacts <- list()

      # Define covariates
      xColNamesAll <- c("AIDS")
      # Define outcomes
      yColNamesAll <- c("Age", "SqCD4", "Transmission", "Migr")

      # Determine which columns to pass to the mice package

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
      splineBasisMatrix <- as.data.table(splines::ns(dataSet$DY, df = nsdf))
      setnames(splineBasisMatrix, paste0("V", colnames(splineBasisMatrix)))
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

        # Not used levels must be removed
        X <- droplevels(X)
        Y <- droplevels(Y)

        # Run model
        cat("Performing imputation.\n")
        imp <- mice::mice(cbind(Y, X),
                          m = nimp,
                          maxit = nit)
        imp <- setDT(mice::complete(imp, action = "long", include = TRUE))
        setnames(imp, old = c(".imp", ".id"), new = c("Imputation", "id"))

      } else {
        imp <- data.table(Imputation = 0L,
                          id = seq_len(nrow(Y)))
      }

      indexColNames <- c("Imputation", "id")
      impColNames <- union(indexColNames, yColNames)
      dataSetColNames <- setdiff(colnames(dataSet), yColNames)

      mi <- cbind(imp[, ..impColNames],
                  dataSet[, ..dataSetColNames])

      setcolorder(mi, union(indexColNames, names(dataSet)))

      ConvertDataTableColumns(mi, c(Imputation = function(x) as.integer(as.character(x))))

      return(list(Table = mi,
                  Artifacts = artifacts))
    }

    # 1. Preprocess data for mice specifically
    preProcData <- PreProcessInputDataForMice(inputData)
    inputData <- preProcData$Table

    # 2. Save original order for later
    inputData[, OrigSort := .I]

    # 3. Measures years from earlier diagnosis year
    inputData[, DY := DateOfDiagnosisYear - min(DateOfDiagnosisYear)]

    # 4. Split by gender to data sets
    dataSets <- split(inputData, by = c("Gender"))

    # 5. Execute the worker function per data set
    if (parameters$runInParallel) {
      # Run in parallel
      cl <- parallel::makeCluster(2)
      parallel::clusterExport(cl, c("libPaths", "mice.impute.pmm", "mice.impute.pmm",
                                    "mice.impute.polyreg", "mice.impute.logreg",
                                    "ConvertDataTableColumns", "dataSets"))
      parallel::clusterEvalQ(cl, {
				.libPaths(libPaths)
        library(data.table)
      })
      outputData <- parallel::parLapply(cl,
                                        seq_along(dataSets),
                                        workerFunction,
                                        nit = parameters$nit,
                                        nimp = parameters$nimp,
                                        nsdf = parameters$nsdf)
      parallel::stopCluster(cl)
    } else {
      # Run sequentially
      outputData <- lapply(seq_along(dataSets),
                           workerFunction,
                           nit = parameters$nit,
                           nimp = parameters$nimp,
                           nsdf = parameters$nsdf)
    }

    # 6. Combine all data sets
    names(outputData) <- names(dataSets)
    table <- rbindlist(lapply(outputData, "[[", "Table"))
    artifacts <- lapply(outputData, "[[", "Artifacts")
    artifacts <- modifyList(artifacts, preProcData$Artifacts)

    # 7. Restore original order per Imputation
    setorder(table, Imputation, OrigSort)

    return(
      list(Table = table,
           Artifacts = artifacts)
    )
  }
)
