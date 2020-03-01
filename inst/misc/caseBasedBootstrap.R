library(data.table)
library(hivModelling)
library(hivEstimatesAccuracy)

# A. DEFINE SETTINGS -------------------------------------------------------------------------------

# Path to case-based data - no missing assumed
caseBasedDataPath <- '~/share/HIV Bootstrap/BE_case_based.csv'

# Name of column in case-based data to prepare data by; for instance "Imputation", if case-based
# data is after adjustments. Then result of "PrepareDataSetsForModel" will be a list with length
# equal to the number of unique values in that column.
splitBy <- NULL

# Vector of column names in the case-based data to be used in "PrepareDataSetsForModel" to split
# aggregate counts of data by.
strata <- NULL

# Vector of population names to aggregate counts in the final input data set for the model.
populationSet <- NULL

# Number of bootstrap iterations
bootstrapCount <- 20

# Common parameters to be used throughout the model runs. Normally, they will be determined from
# data, but this work is not finalized yet.
parameters <- list()


# B. MAIN FIT --------------------------------------------------------------------------------------

# 1. Load case-based data set with no missings
caseBasedData <- fread(caseBasedDataPath)

# 2. Create aggregated data set
hivModelDataSet <- PrepareDataSetsForModel(caseBasedData, splitBy, strata)

# 3. Create context
originalContext <- GetRunContext(data = hivModelDataSet, parameters = parameters)

# 4. Create work data set for the model
data <- GetPopulationData(context, populationSet)

# 5. Fit the model. Arguments "param" and "info" are not provided, therefore a full search for the
# best fitting parameters is carried out.
mainResults <- PerformMainFit(context, data)

# 6. Create and explore output plots (optional)
plots <- CreateOutputPlots(mainResults)
print(plots)

# 7. Use main fit parameters as starting point for the bootstrap fits
paramInit <- mainResults$Param
infoInit <- mainResults$Info

# 8. Check parameters estimated in the main run (optional)

# Parameter "beta" - diagnosis rates
paramInit$Beta

# Parameter "theta" - spline weights
paramInit$Theta


# C. BOOTSTRAP FITS --------------------------------------------------------------------------------

# This can be parallelized. Sequential run is for testing.
# This is basically replicating the same process as applied for the main fit with:
# a) case-based data boostrapped (with replications),
# b) starting point for the search is the set of estimates from the main run.

bootstrapResults <- list()
for (iter in seq_len(bootstrapCount)) {
  # 1. Bootstrap case-based data
  indices <- sample(nrow(caseBasedData), replace = TRUE)
  sampleCaseBasedData <- caseBasedData[indices]

  # 2. Create aggregated data set
  hivModelDataSet <- PrepareDataSetsForModel(sampleCaseBasedData, splitBy, strata)

  # 3. Create context
  context <- GetRunContext(data = hivModelDataSet, parameters = parameters)

  # context <- GetRunContext(data = hivModelDataSet, parameters = list(
  #   ModelMinYear = originalContext$Parameters$INCIDENCE$ModelMinYear,
  #   ModelMaxYear = originalContext$Parameters$INCIDENCE$ModelMaxYear
  # ))

  # context$Parameters$INCIDENCE$ModelMinYear <- 'equal to original'
  # context$Parameters$INCIDENCE$ModelMaxYear <- 'equal to original'

  # 4. Create final data set for the model
  data <- GetPopulationData(context)

  # 5. Fit the model. Arguments "param" and "info" are provided, therefore a search for the
  # best fitting parameters is carried out starting from the supplied starting point for
  # beta and thetaF..
  bootstrapResults[[iter]] <- PerformMainFit(context, data, param = paramInit, info = infoInit)
}


# D. EXPLORE BOOTSTRAP RESULTS ---------------------------------------------------------------------

# For instance, these are results of iteration 1:

# Logical indicating whether converges has been achieved
bootstrapResults[[1]]$Converged

# Best fitting parameter (combination of "beta" and "theta")
bootstrapResults[[1]]$P

# Full "info" variable (details of data related settings)
bootstrapResults[[1]]$Info

# Full "param" variable (details of data-related parameters, including "beta" and "theta")
bootstrapResults[[1]]$Param

# Fit statistics, likelihood, etc.
bootstrapResults[[1]]$Statistics

# Results of each amoeba iteration
bootstrapResults[[1]]$IterResults

# Model results
bootstrapResults[[1]]$ModelResults

# Additional count-based results
bootstrapResults[[1]]$CountResults

# Additional time-based results
bootstrapResults[[1]]$TimeResults

# Final results, as outputted by the Windows version (combination of the above results)
bootstrapResults[[1]]$MainOutputs
# Every iteration has the same set of outputs, just like object "mainResults".


# E. FIT MODEL TO PROVIDED PARAMETERS --------------------------------------------------------------

# This section shows how to evaluate the model with given "beta" and "theta" parameters

# Model parameters

# E1) provided externally ...
beta <- c(0.22418973, 0.19100143, 0.07144893, 0.07608724)
theta <- c(0.0000, 683.7634, 171.1121, 828.2901, 1015.1668, 935.0453, 1058.9732, 1182.9012)

# E2) ... or extracted from "mainResults" object or ...
# beta <- mainResults$Param$Beta
# theta <- mainResults$Param$Theta

# E2) ... extracted from the results of the first iteration of bootstrap
# beta <- bootstrapResults[[1]]$Param$Beta
# theta <- bootstrapResults[[1]]$Param$Theta

# Evaluate model
model <- FitModel(beta, theta, context, data)

# Get model outputs
# Most time-consuming is determining time-related outputs (time to diagnosis)
modelOutputs <- GetModelOutputs(model, data)

# Create output plots
plots <- CreateOutputPlots(modelOutputs)
