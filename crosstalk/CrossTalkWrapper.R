library(jsonlite)
library(ggplot2)
library(gridSVG)
library(corrplot)
library(svglite)
library(directlabels)

source('apcversion2.R')
source('crosstalk.R')

OUTPUT_DIR <- '../tmp/'
dir.create(OUTPUT_DIR)


#-------------------------------------------------------
# parseJSON
# 
# Parses a json string from the client as a list
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list containing calculation parameters
#-------------------------------------------------------
parseJSON <- function(data) {
  
  #todo: remove this block when finished
  #data = fromJSON(txt = 'input_new.json') 
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
  
  toJSON(list(
    
    # Generate Incidence Rates Graphs/Tables
    IncidenceRates = list(
      graphs = list(
        getRatesGraph(results$input$A),
        getRatesGraph(results$input$B)
      ),
      
      tables = list(
        as.data.frame(getRates(results$input$A)),
        as.data.frame(getRates(results$input$B))
      ),
      
      headers = colnames(getRates(results$input$A))
    ),
    
    # Generate Incidence Rate Ratios
    IncidenceRateRatios = list(
      graphs = list(
        getRateRatiosGraph(getRateRatios(results$input$A, results$input$B), T),
        getRateRatiosGraph(getRateRatios(results$input$A, results$input$B))
      ),
      
      tables = list(
        as.data.frame(getRateRatios(results$input$A, results$input$B))
      ),
      
      headers = colnames(getRateRatios(results$input$A, results$input$B))
    ),
    
    # Goodness of Fit
    GoodnessOfFit = list(
      graphs = list(
        plot.apc.resids(results$A),
        plot.apc.resids(results$B)
      ),
      
      tables = c(list(), list()),
      headers = c()
    ),

    # APC of Incidence Rates
    ApcOfIncidenceRates = list(
      
      # Local Drifts
      LocalDrifts = list(
        graphs = list(
          generateRatesGraph(results, 'LocalDrifts'),
          
          generateDualLogGraph(
            
            # Equal Local Drifts
            results$wald$W[7, 3], 
            
            # Equal Net Drifts
            results$wald$W[1, 3])
        ),
        
        tables = list(
          as.data.frame(results$A$LocalDrifts),
          as.data.frame(results$B$LocalDrifts)
        ),
        
        headers = colnames(results$A$LocalDrifts)
      ),
      
      # Net Drifts
      NetDrifts = list(
        tables = list(
          as.data.frame(rbind(
            cbind(Cohort = results$A$Inputs$D$name, results$A$NetDrift), 
            cbind(Cohort = results$B$Inputs$D$name, results$B$NetDrift))
          )
        ),

        headers = c('Cohort', colnames(results$A$NetDrift))
      ),

      # Adjusted Rates
      AdjustedRates = list(
        ComparisonOfAdjustedRates = list(
          
          graphs = list(
            generateTripleLogGraph(
              0, #parallelByCohort
              0, #parallelByPeriod 
              0, #parallelByAge 
              'Parallel By Cohort',
              'Parallel By Period',
              'Parallel By Age'
              ) 
          ),
          
          tables = list(
            as.data.frame(results$wald$W[14:17,])
          ),
          
          headers = colnames(results$wald$W)
        ),
        
        # Fitted Cohort Pattern
        FittedCohortPattern = list(
          graphs = list(
            generateRatesGraph(results, 'FittedCohortPattern')
          ),
          
          tables = list(
            as.data.frame(results$A$FittedCohortPattern),
            as.data.frame(results$B$FittedCohortPattern)
          ),
          
          headers = colnames(results$A$FittedCohortPattern)
        ),
        
        # Fitted Temporal Trends
        FittedTemporalTrends = list(
          graphs = list(
            generateRatesGraph(results, 'FittedTemporalTrends')
          ),
          
          tables = list(
            as.data.frame(results$A$FittedTemporalTrends),
            as.data.frame(results$B$FittedTemporalTrends)
          ),
          
          headers = colnames(results$A$FittedTemporalTrends)
        ),
        
        # Cross-Sectional Age Curve
        CrossSectionalAgeCurve = list(
          graphs = list(
            generateRatesGraph(results, 'CrossAge')
          ),
          
          tables = list(
            as.data.frame(results$A$CrossAge),
            as.data.frame(results$B$CrossAge)
          ),
          
          headers = colnames(results$A$CrossAge)
        )
      )
    ),
    
    # APC of Rate Ratios
    ApcOfRateRatios = list(
      
      # Fitted Cohort Pattern
      FittedCohortPattern = list(
        graphs = list(
          generateRatiosGraph(results, 'FittedCohortPattern')
        ),
        
        tables = list(
          as.data.frame(results$comparison$FVCA$FCP)
        ),
        
        headers = colnames(results$comparison$FVCA$FCP)
      ),
      
      # Fittted Temporal Trends
      FittedTemporalTrends = list(
        graphs = list(
          generateRatiosGraph(results, 'FittedTemporalTrends')
        ),
        
        tables = list(
          as.data.frame(results$comparison$FVPA$FTT)
        ),
        
        headers = colnames(results$comparison$FVPA$FTT)
      ),
      
      # Cross-sectional Age Curve
      CrossSectionalAgeCurve = list(
        graphs = list(
          generateRatiosGraph(results, 'CrossAge')
        ),
        
        tables = list(
          as.data.frame(results$comparison$FVAP$CAC)
        ),
        
        headers = colnames(results$comparison$FVAP$CAC)
      ),
      
      # IO
      IO = list(
        graphs = list(
          generateTripleLogGraph(
            results$wald$W[11,3], #Parallel Cross-Sectional Age Curves
            results$wald$W[12,3], #Parallel Fitted Temporal Trends 
            results$wald$W[13,3], #Parallel Fitted Cohort Pattern 
            'Cross-Sectional Age Curves',
            'Fitted Temporal Trends',
            'Fitted Cohort Pattern'
          ) 
        ),
        
        tables = list(
          as.data.frame(results$comparison$IO)
        ),
        
        headers = colnames(results$comparison$IO)
      )
    )
  ))
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
  
  ggplot(graph, aes(x = period, y = ratio, color = as.factor(age))) +
    geom_line() + 
    geom_point(size = 2.5) + 
    theme_bw() +
    scale_color_discrete(
      guide = F
    ) +
    labs(
      title = data$name,
      x = 'Calendar Periods',
      y = 'Rate per 100000 units'
    ) + 
    geom_dl(
      aes(label = as.factor(age)), 
      method =  list("last.points", hjust = -0.5)
    )
  
  filename = paste0(OUTPUT_DIR, 'RatesGraph_', getTimestamp(), '.svg')
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

  return(apply(output, c(1, 2), function(x) { return (if (is.nan(x) || x == Inf || x == -Inf) 0 else x) }))
}



#-------------------------------------------------------
# Generates a bubble plot from the rate ratios
# Outputs:  (1) The path to the output file
#-------------------------------------------------------
getRateRatiosGraph <- function(output, labels = F) {
  
  filename = paste0(OUTPUT_DIR, 'RatesRatioGraph_', getTimestamp(), '.svg')
  svg(width = 2*ncol(output), height = nrow(output), pointsize = 10 + ncol(output), file = filename)

  min = floor(min(unlist(output)))
  max = ceiling(max(unlist(output)))

  if (labels)
    corrplot(as.matrix(output),
           addCoef.col = "black",
           tl.col="black", tl.srt=45,
           cl.lim = c(min, max),
           is.corr = F)
  else
    corrplot(as.matrix(output),
             tl.col="black", tl.srt=45,
             cl.lim = c(min, max),
             is.corr = F)
  
  dev.off()
  filename
}


#-------------------------------------------------------
# Generates a multiline graph from the rates
# Outputs:  (1) The path to the output file
#-------------------------------------------------------
generateRatesGraph <- function(results, key) {

  resultsA = results$A
  resultsB = results$B
  
  setA = as.data.frame(resultsA[[key]])
  setB = as.data.frame(resultsB[[key]])
  
  setA$key = resultsA$Inputs$D$name
  setB$key = resultsB$Inputs$D$name
  
  filename = paste0(OUTPUT_DIR, key, '_', getTimestamp(), '.svg')

  if (key == 'LocalDrifts') {
    names(setA) = c('Age', 'PercentPerYear', 'CILo', 'CIHi', 'key')
    names(setB) = c('Age', 'PercentPerYear', 'CILo', 'CIHi', 'key')
    title = 'Local Drifts'
    xAxis = 'Age'
    yAxis = 'Percent per Year'
    xMap = 'Age'
    yMap = 'PercentPerYear'
  }

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
  
  plot = ggplot(rbind(setA, setB), mapping)
  
  if (key == 'LocalDrifts') {
    plot = plot + 
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = results$A$NetDrift[,2], ymax = results$A$NetDrift[,3]), alpha = 0.01, linetype = "blank", fill = "#00BFC4") + 
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = results$B$NetDrift[,2], ymax = results$B$NetDrift[,3]), alpha = 0.01, linetype = "blank", fill = "#F8766D") 
  }

  plot = plot +
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
      legend.position = c(0.15, 0.92)
    )
  

  ggsave(file = filename, width = 10, height = 10)
  
  filename
}


generateRatiosGraph <- function(results, key) {
  
  filename = paste0(OUTPUT_DIR, key, '_', getTimestamp(), '.svg')
  
  min = min(results$comparison$IO[,4])
  max = max(results$comparison$IO[,5])
  
  if (key == 'FittedCohortPattern') {
    set = as.data.frame(results$comparison$FVCA$FCP)
    title = 'Fitted Cohort Pattern'
    xAxis = 'Birth Cohort and Calendar Period'
    yAxis = 'Adjusted Rate'
    xMap = 'Coh'
    yMap = 'FCP'
  }
  
  else if (key == 'FittedTemporalTrends') {
    set = as.data.frame(results$comparison$FVPA$FTT)
    title = 'Fitted Temporal Trends'
    xAxis = 'Age'
    yAxis = 'Adjusted Rate'
    xMap = 'Per'
    yMap = 'FTT'
  }
  
  else if (key == 'CrossAge') {
    set = as.data.frame(results$comparison$FVAP$CAC)
    title = 'Cross-Sectional Age Curve'
    xAxis = 'Age'
    yAxis = 'Adjusted Rate'
    xMap = 'Age'
    yMap = 'CAC'
  }
  
  title = paste(results$input$A$name, results$input$B$name)
  mapping = aes_string(x = xMap, y = yMap, ymin = 'CILo', ymax = 'CIHi', color = 'key', fill = 'key')
  
  ggplot(set, mapping) +
    scale_color_discrete(guide = F) +
    scale_fill_discrete(guide = F) +
    guides(color = F) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = min, ymax = max), alpha = 0.1, linetype = "blank", fill = "slategray2") + 
    geom_ribbon(alpha = 0.35) +
    geom_line(alpha = 0.35) +
    geom_point(alpha = 0.7) +
    scale_y_continuous(expand = c(0.2, 0.1)) +
    labs(
      title = title,
      x = xAxis,
      y = yAxis
    ) +
    theme_light()

  ggsave(file = filename, width = 10, height = 10)
  
  filename
}


generateDualLogGraph <- function(A, B) {
  
  logA = -log10(A)
  logB = -log10(B)
  
  logA = if (logA > 16) 16 else logA
  logB = if (logB > 16) 16 else logB
  
  graphA = rbind(data.frame(), c(-10, 1))
  graphA = rbind(graphA, c(logA, 1))
  graphA$group = "Equal Local Drifts"
  names(graphA) = c('x', 'y', 'group')
  
  graphB = rbind(data.frame(), c(-10, 2))
  graphB = rbind(graphB, c(logB, 2))
  graphB$group = "Equal Net Drifts"
  names(graphB) = c('x', 'y', 'group')
  
  graph = rbind(graphA, graphB)

  ggplot(graph, aes(x, y, group = group, fill = group)) + 
    geom_point(color="black", fill = "gold", size = 5, pch = 21) + 
    geom_line(aes(color = group)) + 
    coord_cartesian(xlim = c(0, 20), ylim = c(0.5, 2.5)) + 
    scale_color_discrete(guide = F) +
    theme_bw() + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    geom_vline(xintercept = 1, linetype = "longdash", color = "slategray2") +
    geom_rect(aes(xmin = -Inf, xmax = 1, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "slategray2") + 
    labs(
      title = "Comparison of Drifts",
      x = "-Log10(P-Value)",
      y = ""
    ) +
    geom_dl(
      aes(label = group),
      method = list("last.points", hjust = 0, vjust = -2)
    )

  filename = paste0(OUTPUT_DIR, 'ComparisonOfDrifts', '_', getTimestamp(), '.svg')
  ggsave(file = filename, width = 10, height = 10)
  
  filename
}


generateTripleLogGraph <- function(A, B, C, labelA, labelB, labelC) {
  
  logA = -log10(A)
  logB = -log10(B)
  logC = -log10(C)
  
  logA = if (logA > 16) 16 else logA
  logB = if (logB > 16) 16 else logB
  logC = if (logC > 16) 16 else logC
  
  graphA = rbind(data.frame(), c(-10, 1))
  graphA = rbind(graphA, c(logA, 1))
  graphA$group = labelA
  names(graphA) = c('x', 'y', 'group')
  
  graphB = rbind(data.frame(), c(-10, 2))
  graphB = rbind(graphB, c(logB, 2))
  graphB$group = labelB
  names(graphB) = c('x', 'y', 'group')

  graphC = rbind(data.frame(), c(-10, 3))
  graphC = rbind(graphC, c(logC, 3))
  graphC$group = labelC
  names(graphC) = c('x', 'y', 'group')

  graph = rbind(graphA, graphB, graphC)  
  
  ggplot(graph, aes(x, y, group = group, fill = group)) + 
    geom_point(color="black", fill = "gold", size = 5, pch = 21) + 
    geom_line(aes(color = group)) + 
    coord_cartesian(xlim = c(0, 25), ylim = c(0.5, 3.5)) + 
    scale_color_discrete(guide = F) +
    theme_bw() + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    geom_vline(xintercept = 1, linetype = "longdash", color = "slategray2") +
    geom_rect(aes(xmin = -Inf, xmax = 1, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "slategray2") + 
    labs(
      title = "Comparison of Adjusted Rates",
      x = "-Log10(P-Value)",
      y = ""
    ) +
    geom_dl(
      aes(label = group),
      method = list("last.points", hjust = 0, vjust = -2)
    )

  filename = paste0(OUTPUT_DIR, 'Parallel', '_', getTimestamp(), '.svg')
  ggsave(file = filename, width = 10, height = 10)
  
  filename
}



plot.apc.resids <- function(M) {
  
  par(mfrow = c(1, 2), pty = "m")
  
  a <- M$FittedRates$ages
  A <- length(a)
  p <- M$FittedRates$periods
  P = length(p)
  z <- matrix(M$APCModel$DevResids, nrow = A-1)
  
  XL <- c(min(p) - 0.25*diff(range(p)), max(p)+.05*diff(range(p)))
  
  YL <- c(min(a) - 0.05*diff(range(a)), max(a)+.075*diff(range(a)))
  YL <- rev(YL)
  
  
  ###
  # Heat map of residuals
  ###
  
  filenameA = paste0(OUTPUT_DIR, 'HeatMap_', getTimestamp(), '.svg')
  filenameB = paste0(OUTPUT_DIR, 'QQPlot_', getTimestamp(), '.svg')
  
  svg(height = 10, width = 10, pointsize = 10, file = filenameA)

  image(p, a, t(z), 
        ylim = YL,
        xlim = XL, 
        zlim = c(-3.0, 3.0),
        axes = FALSE, asp = 1,
        col = topo.colors(128, alpha = 1), xlab = "", ylab = "")
  
  axis(2, a[c(seq(from = 1, to = A, by = 5), A)], pos = p[1], las = 2)
  # text(median(p), 1.05*max(a), labels = 'Period', pos = 3, cex = 2, srt = 0)
  
  axis(3, p[c(seq(from = 1, to = P, by = 4), P)], pos = a[1], las = 3)
  # text(XL[1], median(a), labels = 'Age', pos = 1, cex = 2, srt = -90)

  dev.off()

  ###
  # normal probability plot
  ###
  svg(height = 10, width = 10, pointsize = 10, file = filenameB)
  
  par(pty = "s")
  qqnorm(z, xlim = c(-3.5,3.5), ylim = c(-3.5,3.5))
  qqline(z, xlim = c(-3.5,3.5), ylim = c(-3.5,3.5))
  
  # reset
  par(mfrow = c(1, 1), pty = "m")
  
  dev.off()
  
  c(filenameA, filenameB)
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

