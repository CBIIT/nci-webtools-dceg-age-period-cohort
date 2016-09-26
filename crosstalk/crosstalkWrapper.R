require(base64enc,    quietly = T)
require(corrplot,     quietly = T)
require(directlabels, quietly = T)
require(ggplot2,      quietly = T)
require(jsonlite,     quietly = T)

source('rcode/apcversion2.R')
source('rcode/crosstalk.R')

config = list(
  output.dir = 'tmp/'
)

dir.create(config$output.dir)


#-------------------------------------------------------
# calculate
# 
# Input:    The JSON string from the client
# Output:   A JSON string containing tables and paths to 
#           output files for the following sections:
#             - APC of Rates
#             - APC of Rate Ratios
# 
#-------------------------------------------------------
calculate <- function(json) {
  
  input = parse.json(json)
  
  results = list()
  results$A = apc2(input$A)
  results$B = apc2(input$B)
  results$comparison = rrcomp1(results$A, results$B)
  results$wald = apcwaldtests2(results$A, results$B)
  results$input = input
  
  output = list(
    
    # Generate Incidence Rates Graphs/Tables
    IncidenceRates = list(
      graphs = list(
        get.rates.plot(results$input$A, ymax = floor(1.2 * max(get.rates(results$input$A), get.rates(results$input$B)))),
        get.rates.plot(results$input$B, ymax = floor(1.2 * max(get.rates(results$input$A), get.rates(results$input$B))))
      ),
      
      # extra level of nesting for tables displayed together
      tables = list(
        list(
          round(as.data.frame(get.rates(results$input$A)), 2),
          round(as.data.frame(get.rates(results$input$B)), 2)
        )
      )
    ),
    
    # Generate Incidence Rate Ratios Section
    IncidenceRateRatios = list(

      # Two tabs - one for labeled correlation plot, one for unlabeled plot
      Labeled = list(
        graphs = list(
          get.rate.ratios.plot(get.rate.ratios(results$input$A, results$input$B), label = T)
        ),
        tables = list(
          list(
            round(as.data.frame(get.rate.ratios(results$input$A, results$input$B)), 2)
          )
        )
      ),
      Unlabeled = list(
        graphs = list(
          get.rate.ratios.plot(get.rate.ratios(results$input$A, results$input$B), label = F)
        ),
        tables = list(
          list(
            round(as.data.frame(get.rate.ratios(results$input$A, results$input$B)), 2)
          )
        )
      )
    ),
    
    # Goodness of Fit Section
    GoodnessOfFit = list(

      # Two tabs - one for each dataset
      A = list(
        graphs = plot.apc.resids(results$A),
        tables = list(
          list()
        )
      ),
      B = list(
        graphs = plot.apc.resids(results$B),
        tables = list(
          list()
        )
      )
    ),
    
    # APC of Incidence Rates Section
    ApcOfIncidenceRates = list(
      
      # Two tabs - Local Drifts / Adjusted Rates

      # Local Drifts
      LocalDrifts = list(
        graphs = list(
          
          # Data for D3 plot
          list(
            title = 'Local Drifts',
            legends = c(results$A$Inputs$D$name, results$B$Inputs$D$name),
            xlabel = 'Age',
            ylabel = 'Percent per Year',
            xaxis = colnames(results$A$LocalDrifts)[1],
            yaxis = colnames(results$A$LocalDrifts)[2],
            curves = list(
              as.data.frame(results$A$LocalDrifts),
              as.data.frame(results$B$LocalDrifts)
            ),
            
            bands = list(
              c(results$A$NetDrift[, 'CI Lo'], results$A$NetDrift[, 'CI Hi']),
              c(results$B$NetDrift[, 'CI Lo'], results$B$NetDrift[, 'CI Hi'])
            )
          ),
          
          get.parallel.plot(
            rbind(data.frame(category = 'Equal Local Drifts', log = results$wald$W[7, 3]),
                  data.frame(category = 'Equal Net Drifts',   log = results$wald$W[1, 3])),
            title = 'Comparison of Drifts'
          )
        ),
        
        tables = list(
          list(
            as.data.frame(cbind(Cohort = results$A$Inputs$D$name, results$A$NetDrift)),
            round(as.data.frame(results$A$LocalDrifts), 2)
          ),

          list(
            as.data.frame(cbind(Cohort = results$B$Inputs$D$name, results$B$NetDrift)),
            round(as.data.frame(results$B$LocalDrifts), 2)
          )
        ),
        
        headers = colnames(results$A$LocalDrifts)
      ),
      
      # Adjusted Rates
      AdjustedRates = list(
        ComparisonOfAdjustedRates = list(
          
          graphs = list(
            get.parallel.plot(
              rbind(data.frame(category = 'Parallel By Cohort', log = 0),
                    data.frame(category = 'Parallel By Period', log = 0),
                    data.frame(category = 'Parallel By Age',    log = 0)),
              
              title = 'Comparison of Adjusted Rates'
            )
          ),
          
          # display a single table
          # in the first available location in the first pane
          tables = list(
            list(
              round(as.data.frame(results$wald$W[14:17,]), 3)
            )
          ),
          
          headers = colnames(results$wald$W)
        ),
        
        # Fitted Cohort Pattern
        FittedCohortPattern = list(
          
          graphs = list(
            # Data for D3 plot
            list(
              title = 'Fitted Cohort Pattern',
              legends = c(results$A$Inputs$D$name, results$B$Inputs$D$name),
              xlabel = 'Birth Cohort and Calendar Period',
              ylabel = 'Adjusted Rates',
              xaxis = colnames(results$A$FittedCohortPattern)[1],
              yaxis = colnames(results$A$FittedCohortPattern)[2],
              curves = list(
                as.data.frame(results$A$FittedCohortPattern),
                as.data.frame(results$B$FittedCohortPattern)
              ),
              
              bands = list()
            )
          ),
          
          tables = list(
            
            # single table for each pane
            list(
              round(as.data.frame(results$A$FittedCohortPattern), 2)
            ),
            
            list(
              round(as.data.frame(results$B$FittedCohortPattern), 2)
            )
          ),
          
          headers = colnames(results$A$FittedCohortPattern)
        ),
        
        # Fitted Temporal Trends
        FittedTemporalTrends = list(
          
          graphs = list(
            # Data for D3 plot
            list(
              title = 'Fitted Temporal Trends',
              legends = c(results$A$Inputs$D$name, results$B$Inputs$D$name),
              xlabel = 'Year',
              ylabel = 'Adjusted Rates',
              xaxis = colnames(results$A$FittedTemporalTrends)[1],
              yaxis = colnames(results$A$FittedTemporalTrends)[2],
              curves = list(
                as.data.frame(results$A$FittedTemporalTrends),
                as.data.frame(results$B$FittedTemporalTrends)
              ),
              
              bands = list()
            )
          ),
          
          tables = list(
            list(
              round(as.data.frame(results$A$FittedTemporalTrends), 2)
            ),
            list(
              round(as.data.frame(results$B$FittedTemporalTrends), 2)
            )
          ),
          
          headers = colnames(results$A$FittedTemporalTrends)
        ),
        
        # Cross-Sectional Age Curve
        CrossSectionalAgeCurve = list(
          
          graphs = list(
            # Data for D3 plot
            list(
              title = 'Cross-Sectional Age Curve',
              legends = c(results$A$Inputs$D$name, results$B$Inputs$D$name),
              xlabel = 'Age',
              ylabel = 'Adjusted Rates',
              xaxis = colnames(results$A$CrossAge)[1],
              yaxis = colnames(results$A$CrossAge)[2],
              curves = list(
                as.data.frame(results$A$CrossAge),
                as.data.frame(results$B$CrossAge)
              ),
              
              bands = list()
            )
          ),
          
          tables = list(
            list(
              round(as.data.frame(results$A$CrossAge), 2)
            ),
            list(
              round(as.data.frame(results$B$CrossAge), 2)
            )
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
          # Data for D3 plot
          list(
            title = paste(results$input$A$name, 'vs', results$input$B$name),
            legends = as.array(paste(results$A$Inputs$D$name, 'vs', results$B$Inputs$D$name)),
            xlabel = 'Birth Cohort and Calendar Period',
            ylabel = 'Adjusted Rate',
            xaxis = colnames(results$comparison$FVCA$FCP)[1],
            yaxis = colnames(results$comparison$FVCA$FCP)[2],
            curves = list(
              as.data.frame(results$comparison$FVCA$FCP)
            ),
            
            bands = list(
              c(results$comparison$IO['cohort', 'CILo'], results$comparison$IO['cohort', 'CIHi'])
            )
          )
        ),
        
        tables = list(
          list(
            round(as.data.frame(results$comparison$FVCA$FCP), 2)
          )
        ),
        
        headers = colnames(results$comparison$FVCA$FCP)
      ),
      
      # Fitted Temporal Trends
      FittedTemporalTrends = list(
        
        graphs = list(
          # Data for D3 plot
          list(
            title = paste(results$input$A$name, 'vs', results$input$B$name),
            legends = as.array(paste(results$A$Inputs$D$name, 'vs', results$B$Inputs$D$name)),
            xlabel = 'Year',
            ylabel = 'Adjusted Rate',
            xaxis = colnames(results$comparison$FVPA$FTT)[1],
            yaxis = colnames(results$comparison$FVPA$FTT)[2],
            curves = list(
              as.data.frame(results$comparison$FVPA$FTT)
            ),
            
            bands = list(
              c(results$comparison$IO['period', 'CILo'], results$comparison$IO['period', 'CIHi'])
            )
          )
        ),
        
        tables = list(
          list(
            round(as.data.frame(results$comparison$FVPA$FTT), 3)
          )
        ),
        
        headers = colnames(results$comparison$FVPA$FTT)
      ),
      
      # Cross-sectional Age Curve
      CrossSectionalAgeCurve = list(

        graphs = list(
          # Data for D3 plot
          list(
            title = paste(results$input$A$name, 'vs', results$input$B$name),
            legends = as.array(paste(results$A$Inputs$D$name, 'vs', results$B$Inputs$D$name)),
            xlabel = 'Age',
            ylabel = 'Adjusted Rate',
            xaxis = colnames(results$comparison$FVAP$CAC)[1],
            yaxis = colnames(results$comparison$FVAP$CAC)[2],
            curves = list(
              as.data.frame(results$comparison$FVAP$CAC)
            ),
            
            bands = list(
              c(results$comparison$IO['age', 'CILo'], results$comparison$IO['age', 'CIHi'])
            )
          )
        ),
        
        tables = list(
          list(
            round(as.data.frame(results$comparison$FVAP$CAC), 2)
          )
        ),
        
        headers = colnames(results$comparison$FVAP$CAC)
      ),
      
      # IO
      IO = list(
        graphs = list(
          get.parallel.plot(
            rbind(data.frame(category = 'Parallel Cross-Sectional Age Curves', log = results$wald$W[11,3]),
                  data.frame(category = 'Parallel Fitted Temporal Trends ',    log = results$wald$W[12,3]),
                  data.frame(category = 'Parallel Fitted Cohort Pattern ',     log = results$wald$W[13,3])),
            
            title = 'Comparison of Adjusted Rates'
          )
        ),
        
        tables = list(
          list(
            round(as.data.frame(results$comparison$IO), 2)
          )
        ),
        
        headers = colnames(results$comparison$IO)
      )
    )
  )

  downloads = list(
    TextInput = paste0(config$output.dir, 'Input', timestamp(), '.txt'),
    TextOutput = paste0(config$output.dir, 'Output', timestamp(), '.txt'),
    RDataInput = paste0(config$output.dir, 'Input', timestamp(), '.rdata'),
    RDataOutput = paste0(config$output.dir, 'Output', timestamp(), '.rdata')
  )
  
  save(input,   file = downloads$RDataInput)
  save(results, file = downloads$RDataOutput)
  capture.output(print(input), file = downloads$TextInput)
  capture.output(print(results), file = downloads$TextOutput)
  
  toJSON(list(
    output = output,
    downloads = downloads), auto_unbox = T)
}


#-------------------------------------------------------
# parse.json
# 
# Parses a json string from the client as a list
# Inputs:   (1) The JSON string from the client
# Outputs:  (1) A list containing calculation parameters
#-------------------------------------------------------
parse.json <- function(json) {
  
  #todo: remove this block when finished
  #data = fromJSON(txt = 'input_new.json') 
  data = fromJSON(json)
  
  data$interval   = as.numeric(data$interval)
  data$startAge   = as.numeric(data$startAge)
  data$startYear  = as.numeric(data$startYear)
  
  tableA     = as.data.frame(data$file1$table)
  tableB     = as.data.frame(pmax(data$file2$table, 0.1))
  
  sequenceA  = seq_along(tableA) %% 2
  sequenceB  = seq_along(tableB) %% 2
  endAge     = data$startAge  + data$interval * nrow(tableA)
  endYear    = data$startYear + data$interval * ncol(tableA) / 2
  
  list(
    A = list(
      name         = data$title1,
      description  = data$description,
      events       = as.matrix(tableA[sequenceA == 1]),
      offset       = as.matrix(pmax(tableA[sequenceA == 0], 0.1)),
      offset_tick  = 100000,
      ages         = seq(data$startAge,  endAge,  by = data$interval),
      periods      = seq(data$startYear, endYear, by = data$interval)
    ),
    B = list(
      name         = data$title2,
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
# get.rates.plot
# Outputs:  (1) The filename of the generated rates graph
#-------------------------------------------------------
get.rates.plot <- function(data, ymax = 1000) {
  
  interval = diff(data$periods)[1] - 1
  offset_tick = data$offset_tick
  events = data$events
  offset = data$offset
  
  output = as.data.frame(offset_tick * events / offset)

  periods = data$periods[1:ncol(output)]
  ages = data$ages[1:nrow(output)]
  
  graph = data.frame()
  
  for (i in 1:length(ages))
    for (j in 1:length(periods))
      graph = rbind(graph, list(
        age = ages[i],
        period = periods[j],
        ratio = output[i, j]
      ))
  
  get.plot('rates', plot = ggplot(graph, aes(x = period, y = ratio, color = as.factor(age))) +
    geom_line() + 
    geom_point() +
    theme_bw() +
    scale_x_continuous(expand = c(0.1, 0)) +
    scale_y_continuous(limits = c(0, ymax)) +
    scale_color_discrete(
      guide = F
    ) +
    labs(
      title = data$name,
      x = 'Calendar Periods',
      y = 'Rate per 100000 units'
    ) + 
    geom_dl(
      aes(label = {paste0(as.character(age), '-', as.character(age + interval))} ), 
      method =  list("last.points", hjust = -0.15)
    )
  )
}


#-------------------------------------------------------
# Creates a bubble plot from the rate ratios
#
# Input: A data frame containing 
# Outputs:  (1) The path to the output file
#-------------------------------------------------------
get.rate.ratios.plot <- function(output, label = F) {
  
  # get min/max values
  min = floor(min(unlist(output)))
  max = max(ceiling(max(unlist(output))), min + 1)

  get.plot('rate_ratios', 
          height = nrow(output) * 100, 
          width = ncol(output) * 120, 
          pointsize = 12,
          plot = corrplot(as.matrix(output),
            addCoef.col = if (label) "black" else NULL,
            tl.col="black", tl.srt=45,
            cl.lim = c(min, max),
            is.corr = F)
  )
}


#-------------------------------------------------------
# Creates a parallel plot
#
# Input:    (1) A data frame containing category and log value columns
#           (2) The title of the plot
# Outputs:  (1) The path to the generated plot
#-------------------------------------------------------
get.parallel.plot <- function(data, title) {
  
  # set the maximum value to 100
  data$log = pmin(-log10(data$log), 100)

  # generate the plot and return the filename
  get.plot(key = 'parallel', plot = ggplot(data, aes(category, log)) + 
    
    # use geom_lollipop from ggalt
    geom_lollipop(point.colour = "steelblue", point.size = 3) + 
    
    # add some padding
    scale_y_continuous(expand = c(0, 0), limits=c(0, ceiling(1.2 * max(data$log)))) + 
    
    # specify labels
    labs(
      title = title,
      x = NULL,
      y = expression(-log[10](P-Value))
    ) +
    
    # set custom styles
    theme_minimal() + 
    theme(
      panel.grid.major.y = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line.y = element_line(color = "steelblue", size = 0.25),
      axis.text.y = element_text(margin = margin(r = -5, l = 0)),
      plot.margin = unit(rep(30, 4), "pt")
    ) +  
    coord_flip()
  )
}



#-------------------------------------------------------
# get.rates
# Outputs:  (1) A dataframe containing incidence rates
#-------------------------------------------------------
get.rates <- function(data) {
  
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
# Calculates rate ratios
# Input:    Two input lists containing events and offsets tables
# Output:   A data frame containing rate ratios
#-------------------------------------------------------
get.rate.ratios <- function(A, B) {
  
  interval = diff(A$periods)[1] - 1
  
  output = as.data.frame((A$offset_tick*A$events/A$offset) / (B$offset_tick*B$events/B$offset))
  
  periods = A$periods[1:ncol(output)]
  ages = A$ages[1:nrow(output)]
  
  colnames(output) = paste(periods, periods + interval, sep = ' - ')
  rownames(output) = paste(ages, ages + interval, sep = ' - ')
  
  apply(output, c(1, 2), function(x) { return (if (is.nan(x) || x == Inf || x == -Inf) 0 else x) })
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
  
  filenameA = paste0(config$output.dir, 'HeatMap_', timestamp(), '.png')
  filenameB = paste0(config$output.dir, 'QQPlot_', timestamp(), '.png')

  png(pointsize = 10, file = filenameA, width = 600, height = 600)
  
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
  png(pointsize = 10, file = filenameB, width = 600, height = 600)
  
  par(pty = "s")
  qqnorm(z, xlim = c(-3.5,3.5), ylim = c(-3.5,3.5))
  qqline(z, xlim = c(-3.5,3.5), ylim = c(-3.5,3.5))
  
  # reset
  par(mfrow = c(1, 1), pty = "m")
  
  dev.off()
  
  c(
    dataURI(file = filenameA, mime = "image/png"),
    dataURI(file = filenameB, mime = "image/png")
  )
}



#-------------------------------------------------------------------------------
# get.plot
# 
# Function: Draws the specified plot and returns the name of the generated file
#-------------------------------------------------------------------------------
get.plot <- function(key = NULL, 
                     plot = NULL,
                     width = 600,
                     height = 600,
                     pointsize = 10) {

  filename = paste0(key, timestamp(), '.png')
  filepath = paste0(config$output.dir, filename)
  
  png(filepath, width = width, height = height, pointsize = pointsize)
  print(plot)
  dev.off()
  
  dataURI(file = filepath, mime = "image/png")
#  filepath
}



#-------------------------------------------------------
# timestamp
# 
# Function: Returns a timestamp that includes microseconds
# Outputs:  (1) A character vector representing the current system time
#-------------------------------------------------------
timestamp <- function () {
  format(Sys.time(), "_%Y%m%d_%H%M%OS6")
}



#-------------------------------------------------------
# geom_lollipop
# 
# Custom geom from ggalt for log plots
#-------------------------------------------------------
geom_lollipop = function(mapping = NULL, data = NULL, ...,
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
                     
                     points = data
                     points$colour = point.colour
                     points$size = point.size
                     
                     grid::gList(
                       ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
                       ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
                     )
                     
                   },
                   
                   draw_key = draw_key_point
    )
  )
}
