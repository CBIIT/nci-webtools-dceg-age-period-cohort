library(jsonlite)
library(ggplot2)
library(gridSVG)

source('apcversion2.R')
source('crosstalk.R')

OUTPUT_DIR <- "./tmp/"
dir.create(OUTPUT_DIR)


#-------------------------------------------------------
# parseJSON
# 
# Parses a json string from the client as a list
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list containing calculation parameters
#-------------------------------------------------------
parseJSON <- function(data) {
  
  if (length(data) < 2)
    data = fromJSON(txt = 'input_new.json')
  
  else
    data = fromJSON(data)
  
  data$interval   = as.numeric(data$interval)
  data$startAge   = as.numeric(data$startAge)
  data$startYear  = as.numeric(data$startYear)
  
  tableA     = as.data.frame(data$inputfile1$table)
  tableB     = as.data.frame(data$inputfile2$table)
  sequenceA  = seq_along(tableA) %% 2
  sequenceB  = seq_along(tableB) %% 2
  endAge     = data$startAge  + data$interval * nrow(tableA)
  endYear    = data$startYear + data$interval * ncol(tableA) / 2

  list(
    A = list(
      name         = data$inputfile1$title,
      description  = data$description,
      events       = as.matrix(tableA[sequenceA == 1]),
      offset       = as.matrix(tableA[sequenceA == 0]),
      offset_tick  = 100000,
      ages         = seq(data$startAge,  endAge,  by = data$interval),
      periods      = seq(data$startYear, endYear, by = data$interval)
    ),
    B = list(
      name         = data$inputfile2$title,
      description  = data$description,
      events       = as.matrix(tableB[sequenceB == 1]),
      offset       = as.matrix(tableB[sequenceB == 0]),
      offset_tick  = 100000,
      ages         = seq(data$startAge,  endAge,  by = data$interval),
      periods      = seq(data$startYear, endYear, by = data$interval)
    )
  )
}


#-------------------------------------------------------
# getCrossTalkDataJSON
# 
# Function: Returns a JSON representation of the CrossTalk calculation results
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A JSON string that contains the calculation results
#-------------------------------------------------------
process <- function(data) {

  input = parseJSON(data)

  results = list()
  results$A = apc2(input$A)
  results$B = apc2(input$B)
  results$comparison = rrcomp1(results$A, results$B)
  results$wald = apcwaldtests2(results$A, results$B)
  results$input = input
  
  output = resultsTemplate()
  
  for (tab in names(output))
    if (is.null(output[[tab]]$tables))
      
      for (section in names(output[[tab]]))
        if (is.null(output[[tab]][[section]]$tables))
          for (subsection in names(output[[tab]][[section]]))
            output[[tab]][[section]][[subsection]] = retrieveData(results, paste(tab, section, subsection))

        else  
          output[[tab]][[section]] = retrieveData(results, paste(tab, section))
  
    else
      output[[tab]] = retrieveData(results, tab)
  
  output
}

# 
# createOutput <- function(results, output, section) {
#   
#   if (!is.null(output[[section]]$tables))
#     output[[section]] = retrieveData(results, section)
#   
#   else
#     for (subsection in names(output[[section]])) {
#       
#       print (paste(section, subsection))
#   
#       sub = output[[section]][[subsection]]
#       if (class(sub) == 'list' && is.null(sub$tables))
#         createOutput(results, output[[section]], subsection)
#     }
# }



#-------------------------------------------------------
# retrieveData
# 
# Inputs:   (1) A list containing calculation results
#           (2) The type of result to retrieve
#
# Outputs:  (1) A list containing the tables and graph filepaths for the section
#-------------------------------------------------------
retrieveData <- function(results, key) {
  print(key)
  
  output = list()
  
  if (key == 'Incidence Rates') {
    output$tables = list(
      as.data.frame(getRates(results$input$A)),
      as.data.frame(getRates(results$input$B))
    )
    
    output$graphs = list(
      getRatesGraph(results$input$A),
      getRatesGraph(results$input$B)
    )
  }
  
  else if (key == 'Incidence Rate Ratios') {
   output$tables =  list(
     as.data.frame(getRateRatios(results$input))
   )
  }
  
  else if (key == 'APC of Incidence Rates Local Drifts') {
    output$tables = list(
      as.data.frame(results$A$LocalDrifts),
      as.data.frame(results$B$LocalDrifts)
    )
  }
  
  else if (key == 'APC of Incidence Rates Net Drifts') {

    netDriftA = as.data.frame(results$A$NetDrift)
    netDriftB = as.data.frame(results$B$NetDrift)
    
    netDriftA = cbind(Cohort = results$A$Inputs$D$name, netDriftA)
    netDriftB = cbind(Cohort = results$B$Inputs$D$name, netDriftB)

    output$tables = list(
      as.data.frame(rbind(netDriftA, netDriftB))
    )
  }
  
  else if (key == 'APC of Incidence Rates Adjusted Rates Comparison of Adjusted Rates') {
    output$tables = list(
      as.data.frame(results$wald$W[14:17,])
    )
  }
  
  else if (key == 'APC of Incidence Rates Adjusted Rates Fitted Cohort Pattern') {
    output$tables = list(
      as.data.frame(results$A$FittedCohortPattern),
      as.data.frame(results$B$FittedCohortPattern)
    )
  }
  
  else if (key == 'APC of Incidence Rates Adjusted Rates Fitted Temporal Trends') {
    output$tables = list(
      as.data.frame(results$A$FittedTemporalTrends),
      as.data.frame(results$B$FittedTemporalTrends)
    )
  }
  
  else if (key == 'APC of Incidence Rates Adjusted Rates Cross-Sectional Age Curve') {
    output$tables = list(
      as.data.frame(results$A$CrossAge),
      as.data.frame(results$B$CrossAge)
    )
  }
  
  else if (key == 'APC of Rate Ratios Fitted Cohort Pattern') {
    output$tables = list(
      as.data.frame(results$comparison$FVCA$FCP)
    )
  }
  
  else if (key == 'APC of Rate Ratios Fitted Temporal Trends') {
    output$tables = list(
      as.data.frame(results$comparison$FVPA$FTT)
    )
  }
  
  else if (key == 'APC of Rate Ratios Cross-Sectional Age Curve') {
    output$tables = list(
      as.data.frame(results$comparison$FVAP$CAC)
    )
  }
  
  else if (key == 'APC of Rate Ratios IO') {
    output$tables = list(
      as.data.frame(results$comparison$IO)
    )
  }
  
  output$headers = colnames(output$tables[[1]])
  
  output
}



#-------------------------------------------------------
# getRates
# Outputs:  (1) A dataframe containing incidence rates
#-------------------------------------------------------
getRates <- function(data) {
  
  interval = diff(data$periods)[1] - 1
  offset_tick = data$offset_tick
  events = data$events
  offset = data$offset

  output = offset_tick * events / offset
  output = as.data.frame(output)
  
  periods = data$periods[1:ncol(output)]
  ages = data$ages[1:nrow(output)]
  
  colnames(output) = paste(periods, periods + interval, sep = ' - ')
  rownames(output) = paste(ages, ages + interval, sep = ' - ')

  output
}



#-------------------------------------------------------
# getRatesGraph
# Outputs:  (1) The filename of the generated rates graph
#-------------------------------------------------------
getRatesGraph <- function(data) {

  interval = diff(data$periods)[1] - 1
  offset_tick = data$offset_tick
  events = data$events
  offset = data$offset
  
  output = offset_tick * events / offset
  output = as.data.frame(output)
  
  periods = data$periods[1:ncol(output)]
  ages = data$ages[1:nrow(output)]
  
  graph = data.frame()
  
  for (i in 1:length(ages))
    for (j in 1:length(periods))
      graph = rbind(graph, list(
        age = ages[i],
        period = periods[j],
        ratio = output[i,j]
      ))

  graph
  
  ggplot(graph, aes(x = period, y = ratio, group = as.factor(age), color = as.factor(age))) +
    geom_line() + 
    geom_point(size = 2.5) + 
    theme_bw() +
    labs(
      title = data$name,
      x = 'Calendar Periods',
      y = 'Rate per 100000 units'
    ) +
    scale_shape_discrete(
      name = 'Age groups'
    ) 
  
  filename = paste0(OUTPUT_DIR, 'RatesGraph_', getTimestamp(), '.png')
  ggsave(file = filename, width = 10, height = 10)

  filename
}



#-------------------------------------------------------
# Calculates rate ratios
# Outputs:  (1) A data frame containing rate ratios results
#-------------------------------------------------------
getRateRatios <- function(data) {

  interval = diff(data$A$periods)[1] - 1

  output = as.data.frame((data$A$offset/data$A$events) / (data$B$offset/data$B$events))
  
  periods = data$A$periods[1:ncol(output)]
  ages = data$A$ages[1:nrow(output)]
  
  colnames(output) = paste(periods, periods + interval, sep = ' - ')
  rownames(output) = paste(ages, ages + interval, sep = ' - ')
  
  output
}



#-------------------------------------------------------
# getResultsTemplate
# 
# Function: Returns a list template used to hold calculation results
# Outputs:  (1) A list representing the results
#-------------------------------------------------------
resultsTemplate <- function() {
  list(
    `Incidence Rates` = list(
      graphs = c('tmp/incidenceRatesA.png', 'tmp/incidenceRatesB.png'),
      tables = c(list(), list()),
      headers = c()
    ),
    
    `Incidence Rate Ratios` = list(
      graphs = c('tmp/incidenceRateRatiosA.png', 'tmp/incidenceRateRatiosB.png'),
      tables = c(list(), list()),
      headers = c()
    ),
    
    `APC of Incidence Rates` = list(
      `Local Drifts` = list(
        graphs = c('tmp/localDriftsA.svg', 'tmp/localDriftsB.svg'),
        tables = c(list(), list()),
        headers = list()
      ),
      `Net Drifts` = list(
        tables = c(list()),
        headers = list()
      ),
      `Adjusted Rates` = list(
        `Comparison of Adjusted Rates` = list(
          graphs = c('tmp/comparisonOfAdjustedRates.png'),
          tables = c(list(), list()),
          headers = list()
        ),
        `Fitted Cohort Pattern` = list(
          graphs = c('tmp/incidenceRates_FittedCohortPattern.svg'),
          tables = c(list(), list()),
          headers = list()
        ),
        `Fitted Temporal Trends` = list(
          graphs = c('tmp/incidenceRates_FittedTemporalTrends.svg'),
          tables = c(list(), list()),
          headers = list()
        ),
        `Cross-Sectional Age Curve` = list(
          graphs = c('tmp/incidenceRates_CrossSecionalAgeCurve.svg'),
          tables = c(list(), list()),
          headers = list()
        )
      )
    ),
    
    `APC of Rate Ratios` = list(
      `Fitted Cohort Pattern` = list(
        graphs = c('tmp/rateRatios_FittedCohortPattern.svg'),
        tables = c(list()),
        headers = list()
      ),
      `Fitted Temporal Trends` = list(
        graphs = c('tmp/rateRatios_FittedTemporalTrends.svg'),
        tables = c(list()),
        headers = list()
      ),
      `Cross-Sectional Age Curve` = list(
        graphs = c('tmp/rateRatios_CrossSectionalAgeCurve.svg'),
        tables = c(list()),
        headers = list()
      ),  
      `IO` = list(
        tables = c(list()),
        headers = list()
      )
    )
  )
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

