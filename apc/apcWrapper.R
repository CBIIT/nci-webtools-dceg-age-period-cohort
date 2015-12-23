library(jsonlite)
library(xlsx)

source('apc.R')

tmpDirectory <- "./tmp/"

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
              "PeriodRR",
              "CohortRR",
              "LocalDrifts")

## Test data generated from: example_data/ClaytonSchifflers1987StatMed.csv
## testData = '{"title":"Belgium Female Lung Cancer Mortality","description":"Example from: Clayton D. & Schifflers E. Models for temporal variation in cancer rates. I: Age-period and age-cohort models. Stat. Med., 1987; 6:449-467.","startYear":1955,"startAge":25,"interval":5,"count":[[3,2,7,3,10],[11,16,11,10,7],[11,22,24,25,15],[36,44,42,53,48],[77,74,68,99,88],[106,131,99,142,134],[157,184,189,180,177],[193,232,262,249,239],[219,267,323,325,343],[223,250,308,412,358],[198,214,253,338,312]],"population":[[1578947.368,1538461.538,1400000,1578947.368,1428571.429],[1666666.667,1632653.061,1527777.778,1408450.704,1228070.175],[1410256.41,1666666.667,1632653.061,1524390.244,1136363.636],[1348314.607,1392405.063,1660079.051,1568047.337,1221374.046],[1590909.091,1321428.571,1379310.345,1636363.636,1288433.382],[1606060.606,1541176.471,1294117.647,1340887.63,1285988.484],[1515444.015,1533333.333,1490536.278,1255230.126,986072.4234],[1307588.076,1417226.634,1455555.556,1414772.727,999581.765],[1066731.612,1181415.929,1297188.755,1335799.425,1048929.664],[849847.561,902527.0758,1010830.325,1115322.144,930595.269],[591574.5444,636715.2633,688060.9192,773632.4102,690265.4867]],"refYear":-1,"refAge":-1,"refCohort":-1}'
## Generate output with: getApcDataJSON(testData)

#-------------------------------------------------------
# getApcDataJSON
# 
# Function: Returns a JSON representation of the APC calculation results
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A JSON string that contains the calculation results
#-------------------------------------------------------
getApcDataJSON <- function(jsonData) {
  
  # Directory for storing output files
  dir.create(tmpDirectory)
  
  # Generate input and output data
  input   = parseJSON(jsonData)
  output  = runAPC(jsonData)

  # Parse output data to results
  results = list()

  ## Generate Tables
  for (key in c(keys, graphKeys))
    results[[key]]$table = as.data.frame(output[[key]])

  ## Generate Graphs
  for (key in graphKeys)
    results[[key]]$pathToFile = getGraph(output, key)

  ## Generate Raw Input/Output Files
  results $RDataInput  $pathToFile = paste0(tmpDirectory, "APCAnalysis_", getTimestamp() , "_RawInput.RData")
  results $RDataOutput $pathToFile = paste0(tmpDirectory, "APCAnalysis_", getTimestamp() , "_RawOutput.RData")
  results $TextInput   $pathToFile = paste0(tmpDirectory, "APCAnalysis_", getTimestamp() , "_RawInput.txt")
  results $TextOutput  $pathToFile = paste0(tmpDirectory, "APCAnalysis_", getTimestamp() , "_RawOutput.txt")

  save(input,  file = results $RDataInput  $pathToFile)
  save(output, file = results $RDataOutput $pathToFile)
  capture.output(print(input),  file = results $TextInput  $pathToFile)
  capture.output(print(output), file = results $TextOutput $pathToFile)
  
  ## Generate Excel Spreadsheet
  results $Excel $pathToFile = generateExcel(results, input$name)

  return (toJSON(results))
}


#-------------------------------------------------------
# parseJSON
# 
# Function: Creates a list from a JSON string
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list that is used as input for the apc calculation
#-------------------------------------------------------
parseJSON <- function(jsonData) {
  data = fromJSON(jsonData)
  
  table         = as.data.frame(data$table)
  sequence      = seq_along(table) %% 2
  endAge        = data$startAge  + data$interval * nrow(table)
  endYear       = data$startYear + data$interval * ncol(table) / 2
  
  if (data$title == '')
    data$title = 'APC Analysis'
  
  if (data$description == '')
    data$description = paste('Timestamp: ', getTimestamp())
  
  contents = list(
    name        = data$title,
    description = data$description,
    events      = as.matrix(table[sequence == 1]), # odd numbered columns
    offset      = as.matrix(table[sequence == 0]), # even numbered columns
    offset_tick = 100000,
    ages        = seq(data$startAge,  endAge,  by = data$interval),
    periods     = seq(data$startYear, endYear, by = data$interval)
  )
}


#-------------------------------------------------------
# runAPC
# 
# Function: Runs the APC calculation
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list containing the results of the apc calculation
#-------------------------------------------------------
runAPC <- function(jsonData) {
  
  data = fromJSON(jsonData)
  
  if (data$refAge != -1)
    apc(parseJSON(jsonData), RVals = c(data$refAge, data$refYear, data$refCohort))
  
  else
    apc(parseJSON(jsonData))
}


#-------------------------------------------------------
# getGraph
# 
# Function: Creates a graph with the given data and key
# Inputs:   (1) APC output data
#           (2) The graph id
# Outputs:  (1) Path to the output file
#-------------------------------------------------------
getGraph <- function (apcOutput, keyGraphName) {
  imageFileName = paste0(tmpDirectory, keyGraphName, "_", getTimestamp(), ".png")
  
  png(file = imageFileName , units="in", width=8, height=6, res=150)
  line.apc(apcOutput, keyGraphName)
  dev.off()
  
  imageFileName
}


#-------------------------------------------------------
# getTimestamp
# 
# Function: Returns a timestamp that includes microseconds
# Outputs:  (1) A character vector representing the current system time
#-------------------------------------------------------
getTimestamp <- function () {
  format(Sys.time(), "%Y%m%d_%H%M%OS6")
}


#-------------------------------------------------------
# generateExcel
# 
# Function: Generates an Excel document from the results
# Inputs:   (1) APC results data
#           (2) Title of graph
#-------------------------------------------------------
generateExcel <- function(apcOutput, title) {
  
  workbook = createWorkbook()
  
  ## Create Sheets
  for (key in c(graphKeys, keys)) {
    
    currentSheet = createSheet(workbook, sheetName = key)
    
    if (key %in%  c("Waldtests", "Coefficients"))
      addDataFrame(x = round(apcOutput[[key]]$table, 3), sheet = currentSheet)
    
    else
      addDataFrame(x = round(apcOutput[[key]]$table, 3), sheet = currentSheet, row.names = FALSE)
    
    if (key %in% graphKeys)
      addPicture(apcOutput[[key]]$pathToFile, currentSheet, scale = .65, startRow = 1, startColumn = 6)
  }

  pathToFile = paste0(tmpDirectory, title, " ", getTimestamp(), ".xlsx")
  saveWorkbook(workbook, pathToFile)
  
  pathToFile
}