library(data.table)
library(hivModelling)

caseBasedData <- fread('~/share/HIV Bootstrap/Dummy_case_based.csv')
hivModelDataSets <- PrepareDataSetsForModel(caseBasedData, strata = c('Gender', 'Transmission'))
# saveRDS(hivModelDataSets, file = '~/share/HIV Bootstrap/Dummy_aggregated.rds')
# hivModelDataSets <- readRDS(file = '~/share/HIV Bootstrap/Dummy_aggregated.rds')

caseBasedData <- fread('~/share/HIV Bootstrap/BE_case_based.csv')
hivModelDataSets <- PrepareDataSetsForModel(caseBasedData, strata = c('Gender', 'Transmission'))
# saveRDS(hivModelDataSets, file = '~/share/HIV Bootstrap/BE_aggregated.rds')
# hivModelDataSets <- readRDS(file = '~/share/HIV Bootstrap/BE_aggregated.rds')

context <- GetRunContext(
  data = hivModelDataSets[[1]],
  parameters = list(
    INCIDENCE = list(
      ModelMinYear = 1985,
      ModelMaxYear = 2017,
      FitPosMinYear = 1985,
      FitPosMaxYear = 2017,
      FitPosCD4MinYear = 1985,
      FitPosCD4MaxYear = 2013,
      FitAIDSMinYear = 1985,
      FitAIDSMaxYear = 2017,
      FitAIDSPosMinYear = 1985,
      FitAIDSPosMaxYear = 2015,
      Intervals = data.table(
        StartYear = c(1985L, 1996L, 2000L, 2005L, 2010L),
        Jump = c(FALSE, TRUE, FALSE, FALSE, FALSE),
        ChangeInInterval = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        DiffByCD4 = c(FALSE, FALSE, FALSE, FALSE, FALSE)
      )
    )
  )
)

populationData <- GetPopulationData(context)

mainResults <- PerformMainFit(context, populationData)

plots <- CreateOutputPlots(mainResults)
