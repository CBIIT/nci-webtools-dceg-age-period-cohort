library(jsonlite)
source('apc.R')

## Sample input: example_data/ClaytonSchifflers1987StatMed.csv
## input = '{"title":"Belgium Female Lung Cancer Mortality","description":"Example from: Clayton D. & Schifflers E. Models for temporal variation in cancer rates. I: Age-period and age-cohort models. Stat. Med., 1987; 6:449-467.","startYear":1955,"startAge":25,"interval":5,"table":[[3,1578947.368,2,1538461.538,7,1400000,3,1578947.368,10,1428571.429],[11,1666666.667,16,1632653.061,11,1527777.778,10,1408450.704,7,1228070.175],[11,1410256.41,22,1666666.667,24,1632653.061,25,1524390.244,15,1136363.636],[36,1348314.607,44,1392405.063,42,1660079.051,53,1568047.337,48,1221374.046],[77,1590909.091,74,1321428.571,68,1379310.345,99,1636363.636,88,1288433.382],[106,1606060.606,131,1541176.471,99,1294117.647,142,1340887.63,134,1285988.484],[157,1515444.015,184,1533333.333,189,1490536.278,180,1255230.126,177,986072.4234],[193,1307588.076,232,1417226.634,262,1455555.556,249,1414772.727,239,999581.765],[219,1066731.612,267,1181415.929,323,1297188.755,325,1335799.425,343,1048929.664],[223,849847.561,250,902527.0758,308,1010830.325,412,1115322.144,358,930595.269],[198,591574.5444,214,636715.2633,253,688060.9192,338,773632.4102,312,690265.4867]],"reference":null}'
## output = calculate(input)

# Create directory for output files
tmp.dir = './tmp/'
dir.create(tmp.dir)


#-------------------------------------------------------
# calculate
# 
# Function: Returns APC calculation results
# Inputs:   (1) JSON string from the client
# Outputs:  (1) JSON string containing calculation results
#-------------------------------------------------------
calculate = function(json) {
  
  tables = c('NetDrift',
             'Waldtests',
             'Coefficients')
  
  graphs = c('AgeDeviations',
             'PerDeviations',
             'CohDeviations',
             'LongAge',
             'CrossAge',
             'Long2CrossRR',
             'FittedTemporalTrends',
             'PeriodRR',
             'CohortRR',
             'LocalDrifts')

  # Generate input and output data
  input   = parseInput(json)
  output  = runAPC(json)

  # Parse output data
  results = list()

  ## Retrieve tables
  for (key in c(keys, graphs)) {
    results[[key]]$table = as.data.frame(output[[key]])
    results[[key]]$headers = colnames(output[[key]])
    
    ## Add row names
    if (key %in% c('Waldtests', 'Coefficients')) {
      results[[key]]$table = cbind(rownames(output[[key]]), output[[key]])
      results[[key]]$headers = c('', colnames(output[[key]]))
    }
  }

  ## Generate graphs
  for (key in graphs)
    results[[key]]$graph = getGraph(output, key)

  ## Generate raw input/output files
  results[['r-input']]  = paste0(tmp.dir, 'APC_analysis_', getTimestamp() , '_input.rds')
  results[['r-output']] = paste0(tmp.dir, 'APC_analysis_', getTimestamp() , '_output.rds')
  results[['text-input']]   = paste0(tmp.dir, 'APC_analysis_', getTimestamp() , '_input.txt')
  results[['text-output']]  = paste0(tmp.dir, 'APC_analysis_', getTimestamp() , '_output.txt')

  saveRDS(input,  file = results[['r-input']])
  saveRDS(output, file = results[['r-output']])
  capture.output(print(input),  file = results[['text-input']])
  capture.output(print(output), file = results[['text-output']])

  toJSON(results, dataframe = 'values', auto_unbox = T)
}


#-------------------------------------------------------
# parseInput
# 
# Function: Creates parameters from a JSON string
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list containing calculation parameters
#-------------------------------------------------------
parseInput <- function(json) {
  input = fromJSON(json)
  
  endAge        = input$startAge  + input$interval * nrow(input$table)
  endYear       = input$startYear + input$interval * ncol(input$table) / 2

  # APC calculation parameters 
  list(
    name        = input$title,
    description = input$description,
    events      = input$table[, c(T, F)], # odd numbered columns
    offset      = input$table[, c(F, T)], # even numbered columns
    offset_tick = 100000,
    ages        = seq(input$startAge,  endAge,  by = input$interval),
    periods     = seq(input$startYear, endYear, by = input$interval),
    reference   = input$reference
  )
}


#-------------------------------------------------------
# runAPC
# 
# Function: Runs the APC calculation
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list containing the results of the apc calculation
#-------------------------------------------------------
runAPC <- function(json) {
  input = parseJSON(json)
  apc2(input, RVals = input$reference)
}


#-------------------------------------------------------
# getGraph
# 
# Function: Creates a graph with the given data and key
# Inputs:   (1) APC output data
#           (2) The graph id
# Outputs:  (1) Path to the output file
#-------------------------------------------------------
getGraph <- function (output, key) {
  fileName = paste0(tmp.dir, key, '_', getTimestamp(), '.png')
  
  png(file = fileName, width = 600, height = 500, pointsize = 10)
  line.apc(output, key)
  dev.off()
  
  fileName
}


#-------------------------------------------------------
# getTimestamp
# 
# Function: Returns a timestamp that includes microseconds
# Outputs:  (1) A character vector representing the current system time
#-------------------------------------------------------
getTimestamp <- function () {
  format(Sys.time(), '%Y%m%d_%H%M%OS6')
}