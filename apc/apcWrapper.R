library(jsonlite)

tmpDirectory <- "./tmp/"

source('apcversion2.R')

## Test data generated from: example_data/ClaytonSchifflers1987StatMed.csv
testData = '{"title":"Belgium Female Lung Cancer Mortality","description":"Example from: Clayton D. & Schifflers E. Models for temporal variation in cancer rates. I: Age-period and age-cohort models. Stat. Med., 1987; 6:449-467.","startYear":1955,"startAge":25,"interval":5,"count":[[3,2,7,3,10],[11,16,11,10,7],[11,22,24,25,15],[36,44,42,53,48],[77,74,68,99,88],[106,131,99,142,134],[157,184,189,180,177],[193,232,262,249,239],[219,267,323,325,343],[223,250,308,412,358],[198,214,253,338,312]],"population":[[1578947.368,1538461.538,1400000,1578947.368,1428571.429],[1666666.667,1632653.061,1527777.778,1408450.704,1228070.175],[1410256.41,1666666.667,1632653.061,1524390.244,1136363.636],[1348314.607,1392405.063,1660079.051,1568047.337,1221374.046],[1590909.091,1321428.571,1379310.345,1636363.636,1288433.382],[1606060.606,1541176.471,1294117.647,1340887.63,1285988.484],[1515444.015,1533333.333,1490536.278,1255230.126,986072.4234],[1307588.076,1417226.634,1455555.556,1414772.727,999581.765],[1066731.612,1181415.929,1297188.755,1335799.425,1048929.664],[849847.561,902527.0758,1010830.325,1115322.144,930595.269],[591574.5444,636715.2633,688060.9192,773632.4102,690265.4867]],"refYear":-1,"refAge":-1,"refCohort":-1}'

## Creates a list from a JSON string
parseJSON <- function(jsonData) {
  data = fromJSON(jsonData)
  ageRange = data$interval * nrow(data$count)
  periodRange = data$interval * ncol(data$count)
  
  contents = list(
    name        = data$title,
    description = data$description,
    events      = data$count,
    offset      = data$population,
    offset_tick = 100000,
    ages        = seq(data$startAge, data$startAge + ageRange, by = data$interval),
    periods     = seq(data$startYear, data$startYear + periodRange, by = data$interval)
  )
}

## Gets the APC Model 
getApcData <- function(jsonData) {
  
  data = fromJSON(jsonData)
  
  if (data$refYear != -1)
    apc2(parseJSON(jsonData), RVals = c(data$refYear, data$refAge, data$refCohort))

  else
    apc2(parseJSON(jsonData))
}

## Returns a JSON representation of the APC model
getApcDataJSON <- function(jsonData) {
  dir.create(tmpDirectory);
  
  input   = parseJSON(jsonData)
  output  = getApcData(jsonData)
  results = list()

  keys      = c("NetDrift",
                "Waldtests",
                "Coefficients")

  graphKeys = c("AgeDeviations",
                "PerDeviations",
                "CohDeviations",
                "LongAge",
                "CrossAge",
                "Long2CrossRR",
                "FittedTemporalTrends",
                "FittedCohortPattern",
                "PeriodRR",
                "CohortRR",
                "LocalDrifts")
  
  ## Generate Graphs
  for (key in graphKeys)
    results[[key]] = list(
      pathToFile = getGraph(output, key),
      table = output[[key]]
    )
  
  ## Generate tables for Net Drift, Wald tests and Coefficients
  for (key in keys)
    results[[key]] = list(
      table = output[[key]]
    )

  ## Generate Raw Input/Output Files

  results$RStudio = list(
    input = paste0(tmpDirectory, "APCAnalysis", getTimestamp() , "_RawInput.RData"),
    output = paste0(tmpDirectory, "APCAnalysis", getTimestamp() , "_RawOutput.RData")
  )
  
  results$RawText = list(
    input = paste0(tmpDirectory, "APCAnalysis", getTimestamp() , "_RawInput.txt"),
    output = paste0(tmpDirectory, "APCAnalysis", getTimestamp() , "_RawOutput.txt")
  )
  
  save(input, file = results$RStudio$input)
  save(output, file = results$RStudio$output)
  
  capture.output(print(input), file = results$RawText$input)
  capture.output(print(output), file = results$RawText$output)
  
  results$Excel = list(
    pathToFile = ""#generateExcel(output)
  )

  return (toJSON(results))
}


getGraph <- function (apcOutput, keyGraphName) {
  dir.create(tmpDirectory)
  imageFileName = paste0(tmpDirectory, keyGraphName, getTimestamp(), ".png")
  
  png(file = imageFileName , units="in", width=8, height=6, res=150)
  line.apc(apcOutput, keyGraphName)
  dev.off()
  
  imageFileName
}

getTimestamp <- function () {
  format(Sys.time(), "_%Y%m%d_%H%M%OS6")
}