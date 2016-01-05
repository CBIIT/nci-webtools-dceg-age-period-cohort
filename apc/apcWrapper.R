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
## testData = '{"title":"Belgium Female Lung Cancer Mortality","description":"Example from: Clayton D. & Schifflers E. Models for temporal variation in cancer rates. I: Age-period and age-cohort models. Stat. Med., 1987; 6:449-467.","startYear":1955,"startAge":25,"interval":5,"table":[[3,1578947.368,2,1538461.538,7,1400000,3,1578947.368,10,1428571.429],[11,1666666.667,16,1632653.061,11,1527777.778,10,1408450.704,7,1228070.175],[11,1410256.41,22,1666666.667,24,1632653.061,25,1524390.244,15,1136363.636],[36,1348314.607,44,1392405.063,42,1660079.051,53,1568047.337,48,1221374.046],[77,1590909.091,74,1321428.571,68,1379310.345,99,1636363.636,88,1288433.382],[106,1606060.606,131,1541176.471,99,1294117.647,142,1340887.63,134,1285988.484],[157,1515444.015,184,1533333.333,189,1490536.278,180,1255230.126,177,986072.4234],[193,1307588.076,232,1417226.634,262,1455555.556,249,1414772.727,239,999581.765],[219,1066731.612,267,1181415.929,323,1297188.755,325,1335799.425,343,1048929.664],[223,849847.561,250,902527.0758,308,1010830.325,412,1115322.144,358,930595.269],[198,591574.5444,214,636715.2633,253,688060.9192,338,773632.4102,312,690265.4867]],"refYear":-1,"refAge":-1,"refCohort":-1,"cohort":-1}'
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

  ## Retrieve tables
  for (key in c(keys, graphKeys))
    results[[key]]$table = as.data.frame(output[[key]])

  ## Generate graphs
  for (key in graphKeys)
    results[[key]]$pathToFile = getGraph(output, key)

  ## Generate raw input/output files
  results $RDataInput  $pathToFile = paste0(tmpDirectory, "APCAnalysis_", getTimestamp() , "_RawInput.RData")
  results $RDataOutput $pathToFile = paste0(tmpDirectory, "APCAnalysis_", getTimestamp() , "_RawOutput.RData")
  results $TextInput   $pathToFile = paste0(tmpDirectory, "APCAnalysis_", getTimestamp() , "_RawInput.txt")
  results $TextOutput  $pathToFile = paste0(tmpDirectory, "APCAnalysis_", getTimestamp() , "_RawOutput.txt")

  save(input,  file = results $RDataInput  $pathToFile)
  save(output, file = results $RDataOutput $pathToFile)
  capture.output(print(input),  file = results $TextInput  $pathToFile)
  capture.output(print(output), file = results $TextOutput $pathToFile)
  
  ## Generate excel spreadsheet
  results $Excel $pathToFile = generateExcel(results, input$name)

  toJSON(results)
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
  
  # Add default title and description if none were provided

  if (data$title == '')
    data$title = 'APC Analysis'
  
  if (data$description == '')
    data$description = paste('Timestamp: ', getTimestamp())
  
  # Return a list that can be used for the apc calculation 
  list(
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
  
  png(file = imageFileName , units="px", width=600, height=500, res=72, pointsize = 10)
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
  
  ## Iterate through the list of keys
  for (key in c(graphKeys, keys)) {
    
    # Create a sheet and set it as the current working sheet
    currentSheet = createSheet(workbook, sheetName = key)
    
    # These tables have row names
    if (key %in%  c("Waldtests", "Coefficients"))
      addDataFrame(x = round(apcOutput[[key]]$table, 3), sheet = currentSheet)
    
    else
      addDataFrame(x = round(apcOutput[[key]]$table, 3), sheet = currentSheet, row.names = FALSE)
    
    # If this key has a graph, add it to the current sheet
    if (key %in% graphKeys)
      addPicture(apcOutput[[key]]$pathToFile, currentSheet, scale = 0.85, startRow = 1, startColumn = 6)
  }

  pathToFile = paste0(tmpDirectory, title, " ", getTimestamp(), ".xlsx")
  saveWorkbook(workbook, pathToFile)
  
  pathToFile
}