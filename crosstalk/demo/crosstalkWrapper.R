library(jsonlite)

source('apcversion2.R')
source('crosstalk.R')

OUTPUT_DIR <- "./tmp/"
dir.create(OUTPUT_DIR)



SECTION_KEYS = c(
  'Incidence Rates',
  'Incidence Rates Ratios',
  'APC of Incidence Rates',
  'APC of Rate Ratios'
)



keys = c(
  # [section] Incidence Rates
    'IncidenceRatesA',
    'IncidenceRatesB',
  # [end-section] Incidence Rates
  
  # [section] Incidence Rate Ratios
    'IncidenceRateRatios',
  # [end-section] Incidence Rate Ratios
  
  # [section] APC of Incidence Rates
  
    ## [tab] Local Drifts
    'LocalDriftsA',
    'LocalDriftsB',

    ## [tab] Net Drifts
    'NetDrifts',
  
    ## [tab] Adjusted Rates
      #### [section] Comparison of Adjusted Rates
      'ComparisonOfAdjustedRates',

      #### [section] Fitted Cohort Pattern
      'FittedCohortPatternA',
      'FittedCohortPatternB',
  
      #### [section] Fitted Temporal Trends
      'FittedTemporalTrendsA',
      'FittedTemporalTrendsB',

      #### [section] Cross-Sectional Age Curve
      'CrossSectionalAgeCurveA',
      'CrossSectionalAgeCurveB'

    ## [end-tab] Adjusted Rates
  # [end-section] APC of Incidence Rates

    
  # [section] APC of Rate Ratios
)

parseJSON <- function(data) {
  data = fromJSON(txt = 'input_new.json')
  # data = fromJSON(data)
  
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
# Function: Returns a JSON representation of the APC calculation results
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


retrieveData <- function(results, key) {
  print(key)
  
  output = list()
  
  if (key == 'Incidence Rates') {
    output$tables = list(
      as.data.frame(getRates(results$input$A)),
      as.data.frame(getRates(results$input$B))
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
    output$tables = list(
      as.data.frame(rbind(results$A$NetDrift, results$B$NetDrift))
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

getRates <- function(data) {
  
  offset_tick = data$offset_tick
  events = data$events
  offset = data$offset
  
  data = offset_tick * events / offset
  data = as.data.frame(data)
  
  colnames(data) = data$periods[1:ncol(data)]
  rownames(data) = data$ages[1:nrow(data)]
  
  data
}


getRateRatios <- function(data) {
  (data$A$offset/data$A$events) / (data$B$offset/data$B$events)
}



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

