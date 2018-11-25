# Load libraries
library(hivEstimatesAccuracy)
library(data.table)
library(survival)


# A) SETUP ---------------------------------------------------------------------

# Start year
startYear <- 2012.5

# End quarter
endQrt <- 2017.0

inputData <- data.table(
  DiagnosisTime = c(2012.00, 2012.50, 2013.00, 2013.50, 2014.00, 2014.50, 2015.00, 2015.50,
                    2016.00, 2016.50, 2017.00, 2017.50, 2017.75),
  NotificationTime = c(2013.00, 2013.00, 2015.00, 2014.50, 2016.00, 2016.00, 2015.00,
                       2016.00, 2017.00, 2018.25, 2018.00, 2018.00, 2018.00)
)

inputData[, ":="(
  MinNotificationTime = min(NotificationTime, na.rm = TRUE),
  MaxNotificationTime = max(NotificationTime, na.rm = TRUE),
  VarX = 4 * (NotificationTime - DiagnosisTime)
)]


# C) RD ADJUSTMENT -------------------------------------------------------------

# Work on a copy
compData <- copy(inputData)

# Create dimensions to match the weights later
outputData <- copy(compData)
# outputData[, VarT := 4 * (pmin.int(MaxNotificationTime, endQrt) - DiagnosisTime) + 1]

# Filter
compData <- compData[!is.na(DiagnosisTime) & !is.na(NotificationTime)]
compData <- compData[VarX >= 0 &
                       DiagnosisTime >= startYear &
                       NotificationTime <= endQrt]
# compData[, ":="(
#   VarT = 4 * (pmin.int(MaxNotificationTime, endQrt) - DiagnosisTime) + 1,
#   Tf = 4 * (pmin.int(MaxNotificationTime, endQrt) - pmax.int(MinNotificationTime, startYear + 0.25))
# )]
compData[, ":="(
  MinDiagnosisTime = min(DiagnosisTime)
)]
compData[, ":="(
  VarTs = DiagnosisTime - MinDiagnosisTime,
  VarXs = DiagnosisTime - MinDiagnosisTime + VarX
)]
# NOTE: Otherwise survival model complains
compData <- compData[VarXs > VarTs]

model <- compData[, Surv(time = VarTs,
                         time2 = VarXs,
                         event = rep(1, .N))]
fit <- compData[, survfit(model ~ 1)]

plot(fit)

# Recreating stratum variables to assign them to the delay distribution dataset
fitStratum <- data.table(
  Delay = fit$time,
  P = fit$surv,
  Weight = 1/fit$surv,
  Var = fit$std.err^2)
fitStratum[, VarT := max(Delay) - Delay]
fitStratum <- fitStratum[VarT >= 0]

# Create final output object
outputData[fitStratum[, c("VarT", "Weight", "P", "Var"), with = FALSE],
           ":="(
             Weight = Weight,
             P = P,
             Var = Var
           ), on = .(VarT)]
outputData[, ":="(
  MissingData = is.na(Weight) | is.infinite(Weight)
)]
outputData[MissingData == TRUE, ":="(
  Weight = 1,
  P = 1
)]
outputData[is.na(Var), Var := 0]
