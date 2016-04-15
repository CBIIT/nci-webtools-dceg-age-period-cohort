library(jsonlite)
library(ggplot2)
library(gridSVG)
library(corrplot)
library(svglite)

source('apcversion2.R')
source('crosstalk.R')

OUTPUT_DIR <- './tmp/'
dir.create(OUTPUT_DIR)


#-------------------------------------------------------
# parseJSON
#
# Parses a json string from the client as a list
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list containing calculation parameters
#-------------------------------------------------------
parseJSON <- function(data) {

  
  data = fromJSON(data)
  #data = fromJSON(txt = 'input_new.json')

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
            output[[tab]][[section]][[subsection]] = retrieveData(results, paste(tab, section, subsection, sep = '_'))

        else
          output[[tab]][[section]] = retrieveData(results, paste(tab, section, sep = '_'))

    else
      output[[tab]] = retrieveData(results, tab)

  toJSON(output)
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

  if (key == 'IncidenceRates') {
    output$tables = list(
      as.data.frame(getRates(results$input$A)),
      as.data.frame(getRates(results$input$B))
    )

    output$graphs = list(
      getRatesGraph(results$input$A),
      getRatesGraph(results$input$B)
    )
  }

  else if (key == 'IncidenceRateRatios') {

    rateRatios = getRateRatios(results$input$A, results$input$B)
    output$tables =  list(
     as.data.frame(rateRatios)
   )

   output$graphs = list(
     getRateRatiosGraph(rateRatios, T),
     getRateRatiosGraph(rateRatios)
   )
  }

  else if (key == 'ApcOfIncidenceRates_LocalDrifts') {
    output$tables = list(
      as.data.frame(results$A$LocalDrifts),
      as.data.frame(results$B$LocalDrifts)
    )
  }

  else if (key == 'ApcOfIncidenceRates_NetDrifts') {

    netDriftA = as.data.frame(results$A$NetDrift)
    netDriftB = as.data.frame(results$B$NetDrift)

    netDriftA = cbind(Cohort = results$A$Inputs$D$name, netDriftA)
    netDriftB = cbind(Cohort = results$B$Inputs$D$name, netDriftB)

    output$tables = list(
      as.data.frame(rbind(netDriftA, netDriftB))
    )
  }

  else if (key == 'ApcOfIncidenceRates_AdjustedRates_ComparisonOfAdjustedRates') {
    output$tables = list(
      as.data.frame(results$wald$W[14:17,])
    )
    output$graphs = list(

    )
  }

  else if (key == 'ApcOfIncidenceRates_AdjustedRates_FittedCohortPattern') {
    output$tables = list(
      as.data.frame(results$A$FittedCohortPattern),
      as.data.frame(results$B$FittedCohortPattern)
    )

    output$graphs = list(
      generateRatesGraph(results$A, results$B, 'FittedCohortPattern')
    )
  }

  else if (key == 'ApcOfIncidenceRates_AdjustedRates_FittedTemporalTrends') {
    output$tables = list(
      as.data.frame(results$A$FittedTemporalTrends),
      as.data.frame(results$B$FittedTemporalTrends)
    )
    output$graphs = list(
      generateRatesGraph(results$A, results$B, 'FittedTemporalTrends')
    )
  }

  else if (key == 'ApcOfIncidenceRates_AdjustedRates_CrossSectionalAgeCurve') {
    output$tables = list(
      as.data.frame(results$A$CrossAge),
      as.data.frame(results$B$CrossAge)
    )
    output$graphs = list(
      generateRatesGraph(results$A, results$B, 'CrossAge')
    )
  }

  else if (key == 'ApcOfRateRatios_FittedCohortPattern') {
    output$tables = list(
      as.data.frame(results$comparison$FVCA$FCP)
    )
    output$graphs = list(
      generateRatiosGraph(results, 'FittedCohortPattern')
    )
  }

  else if (key == 'ApcOfRateRatios_FittedTemporalTrends') {
    output$tables = list(
      as.data.frame(results$comparison$FVPA$FTT)
    )
    output$graphs = list(
      generateRatiosGraph(results, 'FittedTemporalTrends')
    )
  }

  else if (key == 'ApcOfRateRatios_CrossSectionalAgeCurve') {
    output$tables = list(
      as.data.frame(results$comparison$FVAP$CAC)
    )
    output$graphs = list(
      generateRatiosGraph(results, 'CrossAge')
    )
  }

  else if (key == 'ApcOfRateRatios_IO') {
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
getRateRatios <- function(A, B) {

  interval = diff(A$periods)[1] - 1

  output = as.data.frame((A$offset_tick*A$events/A$offset) / (B$offset_tick*B$events/B$offset))

  periods = A$periods[1:ncol(output)]
  ages = A$ages[1:nrow(output)]

  colnames(output) = paste(periods, periods + interval, sep = ' - ')
  rownames(output) = paste(ages, ages + interval, sep = ' - ')

  output
}

getRateRatiosGraph <- function(output, labels = F) {

#  col1 <- colorRampPalette(c("cyan", "#007FFF", "blue", "#871414", "red", "#FF7F00", "yellow", "white" ))

  filename = paste0(OUTPUT_DIR, 'RatesRatioGraph_', getTimestamp(), '.svg')

  svg(height = 10, width = 10, pointsize = 10, file = filename)

  if (labels)
    corrplot(as.matrix(output), method = "circle",
           addCoef.col = "black",
           tl.col="black", tl.srt=45,
           cl.lim = c(0, ceiling(max(unlist(output)))),
#           col = col1(100), 
           is.corr = F)
  else
    corrplot(as.matrix(output), method = "circle",
             tl.col="black", tl.srt=45,
             cl.lim = c(0, ceiling(max(unlist(output)))),
#             col = col1(100),
             is.corr = F)

  dev.off()

  filename
}

#Fitted
# as.data.frame(results$comparison$FVCA$FCP)
#
# else if (key == 'ApcOfRateRatios_FittedTemporalTrends') {
#     as.data.frame(results$comparison$FVPA$FTT)
#
# else if (key == 'ApcOfRateRatios_CrossSectionalAgeCurve') {
#     as.data.frame(results$comparison$FVAP$CAC)
#

generateRatesGraph <- function(resultsA, resultsB, key) {

  setA = as.data.frame(resultsA[[key]])
  setB = as.data.frame(resultsB[[key]])

  setA$key = resultsA$Inputs$D$name
  setB$key = resultsB$Inputs$D$name

  filename = paste0(OUTPUT_DIR, key, '_', getTimestamp(), '.svg')

  if (key == 'FittedCohortPattern') {

    title = 'Fitted Cohort Pattern'
    xAxis = 'Birth Cohort and Calendar Period'
    yAxis = 'Adjusted Rate'
    xMap = 'Cohort'
    yMap = 'Rate'

    colors = c('#0074D9', '#FF851B')
  }

  else if (key == 'FittedTemporalTrends') {
    title = 'Fitted Temporal Trends'
    xAxis = 'Age'
    yAxis = 'Adjusted Rate'
    xMap = 'Period'
    yMap = 'Rate'

    colors = c('#2ECC40', '#7FDBFF')
  }

  else if (key == 'CrossAge') {
    title = 'Cross-Sectional Age Curve'
    xAxis = 'Age'
    yAxis = 'Adjusted Rate'
    xMap = 'Age'
    yMap = 'Rate'

    colors = c('#FFDC00', '#FF4136')
  }

  mapping = aes_string(x = xMap, y = yMap, ymin = 'CILo', ymax = 'CIHi', group = 'key', col = 'key', fill = 'key')

  ggplot(rbind(setA, setB), mapping) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = c('black', 'black')) +
    geom_ribbon(alpha = 0.35) +
    geom_line(alpha = 0.35) +
    geom_point(alpha = 0.7) +
    scale_y_continuous(expand = c(0.2, 0)) +
    labs(
      title = title,
      x = xAxis,
      y = yAxis
    ) +
    theme_light() +
    theme(
      legend.title = element_blank(),
      legend.position = c(0.1, 0.92)
    )

  ggsave(file = filename, width = 10, height = 10)

  filename
}


generateRatiosGraph <- function(results, key) {

  filename = paste0(OUTPUT_DIR, key, '_', getTimestamp(), '.svg')

  if (key == 'FittedCohortPattern') {
    set = as.data.frame(results$comparison$FVCA$FCP)
    title = 'Fitted Cohort Pattern'
    xAxis = 'Birth Cohort and Calendar Period'
    yAxis = 'Adjusted Rate'
    xMap = 'Coh'
    yMap = 'FCP'

    colors = c('#0074D9', '#FF851B')
  }

  else if (key == 'FittedTemporalTrends') {
    set = as.data.frame(results$comparison$FVPA$FTT)
    title = 'Fitted Temporal Trends'
    xAxis = 'Age'
    yAxis = 'Adjusted Rate'
    xMap = 'Per'
    yMap = 'FTT'

    colors = c('#2ECC40', '#7FDBFF')
  }

  else if (key == 'CrossAge') {
    set = as.data.frame(results$comparison$FVAP$CAC)
    title = 'Cross-Sectional Age Curve'
    xAxis = 'Age'
    yAxis = 'Adjusted Rate'
    xMap = 'Age'
    yMap = 'CAC'

    colors = c('#FFDC00', '#FF4136')
  }
  title = paste(results$input$A$name, results$input$B$name)

  mapping = aes_string(x = xMap, y = yMap, ymin = 'CILo', ymax = 'CIHi', group = 'key', col = 'key', fill = 'key')

  ggplot(set, mapping) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = c('black', 'black')) +
    geom_ribbon(alpha = 0.35) +
    geom_line(alpha = 0.35) +
    geom_point(alpha = 0.7) +
    scale_y_continuous(expand = c(0.2, 0)) +
    labs(
      title = title,
      x = xAxis,
      y = yAxis
    ) +
    theme_light() +
    theme(
      legend.title = element_blank(),
      legend.position = c(0.1, 0.92)
    )

  ggsave(file = filename, width = 10, height = 10)

  filename
}




#-------------------------------------------------------
# getResultsTemplate
#
# Function: Returns a list template used to hold calculation results
# Outputs:  (1) A list representing the results
#-------------------------------------------------------
resultsTemplate <- function() {
  list(
    IncidenceRates = list(
      graphs = c(list(), list()),
      tables = c(list(), list()),
      headers = c()
    ),

    IncidenceRateRatios = list(
      graphs = c(list(), list()),
      tables = c(list(), list()),
      headers = c()
    ),

    ApcOfIncidenceRates = list(
      LocalDrifts = list(
        graphs = c(list(), list()),
        tables = c(list(), list()),
        headers = list()
      ),
      NetDrifts = list(
        tables = c(list()),
        headers = list()
      ),
      AdjustedRates = list(
        ComparisonOfAdjustedRates = list(
          graphs = c(list()),
          tables = c(list(), list()),
          headers = list()
        ),
        FittedCohortPattern = list(
          graphs = c(list()),
          tables = c(list(), list()),
          headers = list()
        ),
        FittedTemporalTrends = list(
          graphs = c(list()),
          tables = c(list(), list()),
          headers = list()
        ),
        CrossSectionalAgeCurve = list(
          graphs = c(list()),
          tables = c(list(), list()),
          headers = list()
        )
      )
    ),

    ApcOfRateRatios = list(
      FittedCohortPattern = list(
        graphs = c(list()),
        tables = c(list()),
        headers = list()
      ),
      FittedTemporalTrends = list(
        graphs = c(list()),
        tables = c(list()),
        headers = list()
      ),
      CrossSectionalAgeCurve = list(
        graphs = c(list()),
        tables = c(list()),
        headers = list()
      ),
      IO = list(
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
