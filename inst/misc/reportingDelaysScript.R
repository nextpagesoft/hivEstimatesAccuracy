# A) SETUP AND DATA LOAD ---------------------------------------------------------------------------

# Load main library
library(hivEstimatesAccuracy)
library(data.table)
library(ggplot2)

# Define input data path
inputDataFilePath <- "d:/Drive/Projects/19. PZH/Scripts/Received/20171013/InputData/EL.csv"

# Read input data
originalData <- ReadDataFile(inputDataFilePath)

# Get preliminary attributes mapping
attrMapping <- GetPreliminaryAttributesMapping(originalData)
# Adjust mapping
attrMapping[["FirstCD4Count"]] <- "cd4_num"

# Pre-process data
inputData <- ApplyAttributesMapping(originalData, attrMapping)
inputDataValidityInfo <- ValidateInputData(inputData)
inputData <- PreProcessInputData(inputData)

# B) REPORTING DELAYES ADJUSTMENT SCRIPT -----------------------------------------------------------

# Work on a copy of input data
startYear <- 2000L

# Create intermediate variables
inputData[, ":="(
  NotificationTime = DateOfNotificationYear + 1/4 * DateOfNotificationQuarter,
  DiagnosisTime = DateOfDiagnosisYear + 1/4 * DateOfDiagnosisQuarter
)]

inputData[, VarX := 4 * (NotificationTime - DiagnosisTime)]

inputData[VarX < 0, VarX := NA]

inputData[, ":="(
  MinNotificationTime = min(NotificationTime),
  MaxNotificationTime = max(NotificationTime)
),
by = .(ReportingCountry)]

inputData[, ":="(
  VarT = 4 * (MaxNotificationTime - DiagnosisTime),
  ReportingDelay = 1L,
  Tf = 4 * (MaxNotificationTime - max(startYear, MinNotificationTime))
)]

inputData[, ":="(
  VarXs = Tf - VarX,
  VarTs = Tf - VarT
)]

# Fit a survival model
model <- inputData[, survival::Surv(time = VarTs,
                                    time2 = VarTs + VarXs,
                                    event = ReportingDelay)]
fit <- survival::survfit(model ~ 1)

final <- data.table(Delay = fit$time,
                    P = fit$surv,
                    StErr = fit$std.err,
                    Var = fit$std.err^2)
final[, Delay := max(Delay) - Delay - 1]
final <- final[Delay >= 0]

# Create output object
agregat <- inputData[, .(Count = .N),
                     by = .(DiagnosisTime, ReportingCountry, VarT)]
agregat <- merge(agregat, final,
                 by.x = c("VarT"),
                 by.y = c("Delay"))
agregat[, ":="(
  EstCount = Count / P,
  EstCountVar = (Count * (Count + 1) / P^4 * Var) + Count * (1 - P) / P^2
)]
agregat[, ":="(
  LowerEstCount = EstCount - 1.96 * sqrt(EstCountVar),
  UpperEstCount = EstCount + 1.96 * sqrt(EstCountVar)
)]

# Create output plot
agregatPlot <- ggplot2::ggplot(agregat, aes(x = DiagnosisTime)) +
  ggplot2::geom_ribbon(aes(ymin = pmin(pmax(LowerEstCount, 0), max(EstCount)),
                           ymax = pmin(pmax(UpperEstCount, 0), max(EstCount)),
                           fill = "Uncertainty interval\nfor estimated total count"),
                       alpha = 0.4) +
  ggplot2::geom_point(aes(y = Count, color = "Reported count"), size = 1) +
  ggplot2::geom_point(aes(y = EstCount, color = "Estimated total count"), size = 1) +
  scale_fill_manual("", values = "grey70") +
  scale_colour_manual("", values = c("red", "blue")) +
  ggtitle("Reported and estimated total count of cases") +
  xlab("Reporting date") +
  ylab("Count of HIV cases")

print(agregatPlot)
