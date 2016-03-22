library(stringr)
library(jsonlite)
library(corrplot)

source('apcversion2.R')
source('crosstalk.R')

csvA = 'Urban China.csv'
csvB = 'NHW.csv'

# generatePlots(csvA, csvB)

generatePlots <- function(csvA, csvB) {
  
  resultsA = apc2(parseJSON(csvToJSON(csvA)))
  resultsB = apc2(parseJSON(csvToJSON(csvB)))
  comparison = rrcomp1(resultsA, resultsB)

  # Rates
  matplot(getRates(csvA), type=c("b"), pch = 1, col = 1:9)
  matplot(getRates(csvB), type=c("b"), pch = 1, col = 1:9)
  
  # Rate Ratios
  corrplot(getRateRatios(csvB, csvA), method = "circle", addCoef.col = "black")
  
  
  ########## APC of Rates ##########
  
  # Local Drifts
  resultsA$LocalDrifts
  resultsB$LocalDrifts
  
  ggplot(as.data.frame(resultsA$LocalDrifts), aes(Age)) + geom_line(aes(y = `Percent per Year`)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1)  + geom_point(aes(y = `Percent per Year`))
  ggplot(as.data.frame(resultsB$LocalDrifts), aes(Age)) + geom_line(aes(y = `Percent per Year`)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1)  + geom_point(aes(y = `Percent per Year`))
  
  
  # Net Drifts
  resultsA$NetDrift
  resultsB$NetDrift
  
  
  ## Comparision of Adjusted Rates: TODO
  
  ## Generate Comparison
  
  
  # Fitted Cohort Pattern
  fcpA = as.data.frame(resultsA$FittedCohortPattern)
  fcpB = as.data.frame(resultsB$FittedCohortPattern)
  
  ggplot() +
    geom_point(data = fcpA, aes(x = Cohort, y = Rate)) +
    geom_line(data = fcpA, aes(x = Cohort, y = Rate)) +
    geom_ribbon(data = fcpA, aes(x = Cohort, ymin = CILo, ymax = CIHi), alpha = 0.1, fill = "blue") +
    
    geom_point(data = fcpB, aes(x = Cohort, y = Rate)) +
    geom_line(data = fcpB, aes(x = Cohort, y = Rate)) +
    geom_ribbon(data = fcpB, aes(x = Cohort, ymin = CILo, ymax = CIHi), alpha = 0.1, fill = "red")

  
  # Fitted Temporal Trends
  fttA = as.data.frame(resultsA$FittedTemporalTrends)
  fttB = as.data.frame(resultsB$FittedTemporalTrends)
  
  ggplot() +
    geom_point(data = fttA, aes(x = Period, y = Rate)) +
    geom_line(data = fttA, aes(x = Period, y = Rate)) +
    geom_ribbon(data = fttA, aes(x = Period, ymin = CILo, ymax = CIHi), alpha = 0.1, fill = "blue") +
  
    geom_point(data = fttB, aes(x = Period, y = Rate)) +
    geom_line(data = fttB, aes(x = Period, y = Rate)) +
    geom_ribbon(data = fttB, aes(x = Period, ymin = CILo, ymax = CIHi), alpha = 0.1, fill = "red")
    

  # Cross-Sectional Age Curve
  cacA = as.data.frame(resultsA$CrossAge)
  cacB = as.data.frame(resultsB$CrossAge)
  
  ggplot() +
    geom_point(data = cacA, aes(x = Age, y = Rate)) +
    geom_line(data = cacA, aes(x = Age, y = Rate)) +
    geom_ribbon(data = cacA, aes(x = Age, ymin = CILo, ymax = CIHi), alpha = 0.1, fill = "blue") +
    
    geom_point(data = cacB, aes(x = Age, y = Rate)) +
    geom_line(data = cacB, aes(x = Age, y = Rate)) +
    geom_ribbon(data = cacB, aes(x = Age, ymin = CILo, ymax = CIHi), alpha = 0.1, fill = "red")

  ########## APC of Rate Ratios ##########
  
  # Fitted Cohort Pattern
  ggplot(as.data.frame(comparison$FVCA$FCP), aes(Coh)) + geom_line(aes(y = FCP)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + geom_point(aes(y = FCP))
  
  # Fitted Temporal Trends
  ggplot(as.data.frame(comparison$FVPA$FTT), aes(Per)) + geom_line(aes(y = FTT)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + geom_point(aes(y = FTT))

  # Cross-Sectional Age Curve
  ggplot(as.data.frame(comparison$FVAP$CAC), aes(Age)) + geom_line(aes(y = CAC)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + geom_point(aes(y = CAC))
  
  # Intercepts
  comparison$IO
}


#################################################
## Reads a csv file and returns a matrix of rates
#################################################
getRates <- function(csvFile) {
  
  contents = parseJSON(csvToJSON(csvFile))
  offset_tick = contents$offset_tick
  events = contents$events
  offset = contents$offset
  
  data = offset_tick * events / offset
  
  # for matplot
  matrix(unlist(data), nrow = ncol(data), byrow = T)
}


#################################################
## Reads Two csv files and returns a matrix of rate ratios
#################################################

getRateRatios <- function(csvFileA, csvFileB) {
  
  a = parseJSON(csvToJSON(csvFileA))
  b = parseJSON(csvToJSON(csvFileB))
  
  (a$offset/a$events) / (b$offset/b$events)
}


#################################################
## Creates a list from a JSON string
#################################################
parseJSON <- function(data) {
  data = fromJSON(data)
  
  table     = as.data.frame(data$table)
  sequence  = seq_along(table) %% 2
  endAge    = data$startAge  + data$interval * nrow(table)
  endYear   = data$startYear + data$interval * ncol(table) / 2
  
  list(
    name         = data$title,
    description  = data$description,
    events       = as.matrix(table[sequence == 1]),
    offset       = as.matrix(table[sequence == 0]),
    offset_tick  = 100000,
    ages         = seq(data$startAge,  endAge,  by = data$interval),
    periods      = seq(data$startYear, endYear, by = data$interval)
  )
}


#################################################
## Reads a csv file as a JSON object identical
## to the one generated by the client
#################################################
csvToJSON <- function(csvFile) {
  headers = lapply(read.csv(csvFile, nrow = 5, header = F, stringsAsFactors = F)[[1]], function(header) 
    str_trim(regmatches(header, regexpr(":", header), invert = T)[[1]][2]))
  
  toJSON(list(
    title       = as.character(headers[1]),
    description = as.character(headers[2]),
    startYear   = as.numeric(headers[3]),
    startAge    = as.numeric(headers[4]),
    interval    = as.numeric(headers[5]),
    table       = (read.csv(csvFile, skip = 5, header = F))
  ), dataframe = 'values', auto_unbox = T)
}

