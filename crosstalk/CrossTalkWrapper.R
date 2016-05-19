library(grid)
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
  
  tableA     = as.data.frame(apply(data$inputfile1$table, c(1, 2), function(x) if (x == 0) 0.5 else x))
  tableB     = as.data.frame(apply(data$inputfile2$table, c(1, 2), function(x) if (x == 0) 0.5 else x))
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
# Function: Returns JSON containing the CrossTalk calculation results
# Input:    The JSON string from the client
# Output:   A JSON string containing tables and paths to output files
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
    
    # Generate Incidence Rate Ratios Section
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
    
    # Goodness of Fit Section
    GoodnessOfFit = list(
      graphs = list(
        plot.apc.resids(results$A),
        plot.apc.resids(results$B)
      ),
      
      tables = c(list(), list()),
      headers = c()
    ),

    # APC of Incidence Rates Section
    ApcOfIncidenceRates = list(
      
      # Local Drifts
      LocalDrifts = list(
        graphs = list(
          generateRatesGraph(results, 'LocalDrifts'),
          generateParallelGraph(
            rbind(data.frame(category = 'Equal Local Drifts', log = results$wald$W[7, 3]),
                  data.frame(category = 'Equal Net Drifts', log = results$wald$W[1, 3])),
            title = 'Comparison of Drifts'
          )
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
            generateParallelGraph(
              rbind(data.frame(category = 'Parallel By Cohort', log = 0),
                    data.frame(category = 'Parallel By Period', log = 0),
                    data.frame(category = 'Parallel By Age', log = 0)),
            
              title = 'Comparison of Adjusted Rates'
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
    
    # APC of Rate Ratios Section
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
      
      # Fitted Temporal Trends
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
          generateParallelGraph(
            rbind(data.frame(category = 'Parallel Cross-Sectional Age Curves', log = results$wald$W[11,3]),
                  data.frame(category = 'Parallel Fitted Temporal Trends ', log = results$wald$W[12,3]),
                  data.frame(category = 'Parallel Fitted Cohort Pattern ', log = results$wald$W[13,3])),
            
            title = 'Comparison of Adjusted Rates'
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
    geom_tooltip(size = 3) +
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
# Input:    Two input lists containing events and offsets tables
# Output:   A data frame containing rate ratios
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
# Input: A data frame containing 
# Outputs:  (1) The path to the output file
#-------------------------------------------------------
getRateRatiosGraph <- function(output, label = F) {
  
  min = floor(min(unlist(output)))
  max = ceiling(max(unlist(output)))
  
  if (min == max) 
    min = min - 1

  filename = paste0(OUTPUT_DIR, 'RatesRatioGraph_', getTimestamp(), '.svg')
  svg(width = 2*ncol(output), height = nrow(output), pointsize = 8 + ncol(output), file = filename)

  corrplot(as.matrix(output),
         addCoef.col = if (label) "black" else NULL,
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
  
  setA = as.data.frame(results$A[[key]])
  setB = as.data.frame(results$B[[key]])
  
  setA$key = results$A$Inputs$D$name
  setB$key = results$B$Inputs$D$name
  
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
  }
  
  else if (key == 'FittedTemporalTrends') {
    title = 'Fitted Temporal Trends'
    xAxis = 'Year'
    yAxis = 'Adjusted Rate'
    xMap = 'Period'
    yMap = 'Rate'
  }
  
  else if (key == 'CrossAge') {
    title = 'Cross-Sectional Age Curve'
    xAxis = 'Age'
    yAxis = 'Adjusted Rate'
    xMap = 'Age'
    yMap = 'Rate'
  }
  
  mapping = aes_string(x = xMap, y = yMap, ymin = 'CILo', ymax = 'CIHi', group = 'key', col = 'key', fill = 'key')
  plot = ggplot(rbind(setA, setB), mapping)
  
  if (key == 'LocalDrifts') {
    start = setA[1,1]
    end = setA[nrow(setA), 1]

    plot = plot + 
      geom_rect(aes(xmin = start, xmax = end, ymin = results$A$NetDrift[,2], ymax = results$A$NetDrift[,3]), alpha = 0.01, color = "#F8766D", fill = "#F8766D") + 
      geom_rect(aes(xmin = start, xmax = end, ymin = results$B$NetDrift[,2], ymax = results$B$NetDrift[,3]), alpha = 0.01, color = "#00BFC4", fill = "#00BFC4") 
  }

  plot = plot +
    geom_ribbon(alpha = 0.35) +
    geom_line(alpha = 0.35) +
    geom_tooltip(size = 3) +
    scale_y_continuous(expand = c(0.2, 0)) +
    labs(
      title = title,
      x = xAxis,
      y = yAxis
    ) +
    theme_light() +
    theme(
      legend.title = element_blank()
    )

  print(plot)
  grid.export(name = filename, strict = F, xmldecl = NULL)
  filename
}


generateRatiosGraph <- function(results, key) {
  
  filename = paste0(OUTPUT_DIR, key, '_', getTimestamp(), '.svg')
  min = min(results$comparison$IO[,4])
  max = max(results$comparison$IO[,5])
  
  if (key == 'FittedCohortPattern') {
    set = as.data.frame(results$comparison$FVCA$FCP)
    start = set[1,1]
    end = set[nrow(set), 1]
    title = 'Fitted Cohort Pattern'
    xAxis = 'Birth Cohort and Calendar Period'
    yAxis = 'Adjusted Rate'
    xMap = 'Coh'
    yMap = 'FCP'
    
    color = '#0375B4'
  }
  
  else if (key == 'FittedTemporalTrends') {
    set = as.data.frame(results$comparison$FVPA$FTT)
    start = set[1,1]
    end = set[nrow(set), 1]
    title = 'Fitted Temporal Trends'
    xAxis = 'Year'
    yAxis = 'Adjusted Rate'
    xMap = 'Per'
    yMap = 'FTT'
    
    color = '#4ECDC4'
  }
  
  else if (key == 'CrossAge') {
    set = as.data.frame(results$comparison$FVAP$CAC)
    start = set[1,1]
    end = set[nrow(set), 1]
    title = 'Cross-Sectional Age Curve'
    xAxis = 'Age'
    yAxis = 'Adjusted Rate'
    xMap = 'Age'
    yMap = 'CAC'
    
    color = '#C7F464'
  }
  
  title = paste(results$input$A$name, 'vs', results$input$B$name)
  mapping = aes_string(x = xMap, y = yMap, ymin = 'CILo', ymax = 'CIHi', color = 'key', fill = 'key', group = 'key')

  plot = ggplot(set, mapping) +
    geom_ribbon(alpha = 0.35) +
    geom_line(alpha = 0.35) +
    geom_rect(aes(xmin = start, xmax = end, ymin = min, ymax = max), alpha = 0.01) + 
    geom_tooltip(size = 3) +
    
    scale_y_continuous(expand = c(0.2, 0.1)) +
    labs(
      title = title,
      x = xAxis,
      y = yAxis
    ) +
    theme_light() +
    theme(
      legend.position = "none"
    )
  
  print(plot)
  grid.export(name = filename, strict = F, xmldecl = NULL)
  filename
}



#-------------------------------------------------------
# Creates a parallel plot
#
# Input:    (1) A data frame containing category and log value columns
#           (2) The title of the plot
# Outputs:  (1) The path to the generated plot
#-------------------------------------------------------
generateParallelGraph <- function(data, title) {
  
  data$log = pmin(-log10(data$log), 100)

  ggplot(data, aes(category, log)) + 
    geom_lollipop(point.colour = "steelblue", point.size = 3) + 
    scale_y_continuous(expand = c(0, 0), limits=c(0, ceiling(1.2 * max(data$log)))) + 

    labs(
      title = title,
      x = NULL,
      y = expression(-log[10](P-Value))
    ) +

    theme_minimal() + 
    theme(
      panel.grid.major.y = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line.y = element_line(color = "steelblue", size = 0.25),
      axis.text.y=element_text(margin=margin(r=-5, l=0)),
      plot.margin=unit(rep(30, 4), "pt")
    ) +  
    coord_flip() 
  
  filename = paste0(OUTPUT_DIR, 'Parallel', '_', getTimestamp(), '.svg')
  ggsave(file = filename, width = 10, height = 10)
  
  filename
}



#-------------------------------------------------------
# Creates the graphs for the goodness of fit section
# Outputs:  Returns the filepaths of the generated graphs
#-------------------------------------------------------
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
# Modifies geom_point to add a tooltip data attribute to each point
# Outputs:  (1) a modified geom_ object
#-------------------------------------------------------
geom_tooltip = function(...) {
  
  point = geom_point(...)
  
  point$geom = ggproto('geom_tooltip', point$geom,
    draw_panel = function(self, data, ...) {
     grobs = list()

     for (i in 1:nrow(data)) {

       row = data[i,]
       title = paste('X:', row$x, '<br />Y:', row$y, '<br />CILo:', row$ymin, '<br />CIHi:', row$ymax)
       grob = ggproto_parent(GeomPoint, self)$draw_panel(data = row, ...)
       grobs[[i]] = garnishGrob(grob, `data-toggle` = "tooltip", `title` = title)
     }


     ggplot2:::ggname('geom_tooltip', gTree(
       children = do.call('gList', grobs)
     ))
    }
  )

  point
}

#-------------------------------------------------------
# Use geom_lollipop from ggalt
#-------------------------------------------------------
geom_lollipop <- function(mapping = NULL, data = NULL, ...,
                          horizontal = FALSE,
                          point.colour = NULL, point.size = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      horizontal = horizontal,
      point.colour = point.colour,
      point.size = point.size,
      ...
    ),
    geom = ggproto("GeomLollipop", Geom,
       required_aes = c("x", "y"),
       non_missing_aes = c("size", "shape", "point.colour", "point.size", "horizontal"),
       default_aes = aes(
         shape = 19, colour = "black", size = 0.5, fill = NA,
         alpha = NA, stroke = 0.5
       ),
       
       setup_data = function(data, params) {
         if (params$horizontal) {
           transform(data, yend = y, xend = 0)
         } else {
           transform(data, xend = x, yend = 0)
         }
       },
       
       draw_group = function(data, panel_scales, coord,
                             point.colour = NULL, point.size = NULL,
                             horizontal = FALSE) {
         
         points <- data
         points$colour <- point.colour
         points$size <- point.size
         
         gList(
           ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
           ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
         )
         
       },
       
       draw_key = draw_key_point
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

