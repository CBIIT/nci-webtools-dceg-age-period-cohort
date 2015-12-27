library(jsonlite)

source('apcversion2.R')

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


#-------------------------------------------------------
# getCrossTalkDataJSON
# 
# Function: Returns a JSON representation of the APC calculation results
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A JSON string that contains the calculation results
#-------------------------------------------------------
getCrossTalkDataJSON <- function(jsonData) {
  
  # Directory for storing output files
  dir.create(tmpDirectory)
  
  # Generate input and output data
  input   = parseJSON(jsonData)

}


#-------------------------------------------------------
# parseJSON
# 
# Function: Creates a list from a JSON string
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list that is used as input for the apc calculation
#-------------------------------------------------------
parseJSON <- function(jsonData) {

  contents = c()
  
  for (data in fromJSON(jsonData)) {
    table         = as.data.frame(data$table)
    sequence      = seq_along(table) %% 2
    endAge        = data$startAge  + data$interval * nrow(table)
    endYear       = data$startYear + data$interval * ncol(table) / 2
    
    if (data$title == '')
      data$title = 'CrossTalk Analysis'
    
    if (data$description == '')
      data$description = paste('Timestamp: ', getTimestamp())
    
    contents = c(contents,
      name        = data$title,
      description = data$description,
      events      = as.matrix(table[sequence == 1]), # odd numbered columns
      offset      = as.matrix(table[sequence == 0]), # even numbered columns
      offset_tick = 100000,
      ages        = seq(data$startAge,  endAge,  by = data$interval),
      periods     = seq(data$startYear, endYear, by = data$interval)
    )
  }
  
  contents
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

