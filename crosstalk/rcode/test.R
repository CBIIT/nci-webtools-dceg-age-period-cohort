library(stringr)
library(jsonlite)
library(corrplot)
library(ggplot2)
library(reshape2)
library(gridSVG)

source('apcversion2.R')
source('crosstalk.R')

OUTPUT_DIR = './tmp/'

csvA = 'Urban China.csv'
csvB = 'NHW.csv'

# generatePlots(csvA, csvB)

# processRequest(clientJSON)

#-------------------------------------------------------
# getTimestamp
# 
# Function: Returns a timestamp that includes microseconds
# Outputs:  (1) A character vector representing the current system time
#-------------------------------------------------------
getTimestamp <- function () {
  format(Sys.time(), "%Y%m%d_%H%M%OS6")
}





generateRatesGraph <- function(resultsA, resultsB, key) {
  
  setA = as.data.frame(resultsA[[key]])
  setB = as.data.frame(resultsB[[key]])

  setA$key = resultsA$Inputs$D$name
  setB$key = resultsB$Inputs$D$name

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

  plot = ggplot(rbind(setA, setB), mapping) +
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
  
  plot
}


processRequest <- function(data) {
  data = parseClientJSON(data)
  response = list()
  
  resultsA = apc2(data$a)
  resultsB = apc2(data$b)
  comparison = rrcomp1(resultsA, resultsB)
  wald = apcwaldtests2(resultsA, resultsB)
  
  
  # Rates
  matplot(getRates(csvA), type=c("b"), pch = 1, col = 1:9)
  matplot(getRates(csvB), type=c("b"), pch = 1, col = 1:9)
  
  # Rate Ratios
  corrplot(getRateRatios(csvB, csvA), addCoef.col = "black")
  
  
  ########## APC of Rates ##########
  
  # Local Drifts
  resultsA$LocalDrifts
  resultsB$LocalDrifts
  
  ggplot(as.data.frame(resultsA$LocalDrifts), aes(Age)) + geom_line(aes(y = `Percent per Year`)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1)  + geom_point(aes(y = `Percent per Year`))
  ggplot(as.data.frame(resultsB$LocalDrifts), aes(Age)) + geom_line(aes(y = `Percent per Year`)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1)  + geom_point(aes(y = `Percent per Year`))
  
  
  # Net Drifts
  resultsA$NetDrift
  resultsB$NetDrift
  
  
  # Comparision of Adjusted Rates
  wald$W[14:17,]
  ## Generate Comparison 'Stemp plot
  
  
  # Fitted Cohort Pattern
  resultsA$FittedCohortPattern
  resultsB$FittedCohortPattern
  generateRatesGraph(resultsA, resultsB, 'FittedCohortPattern')
  
  
  # Fitted Temporal Trends
  resultsA$FittedTemporalTrends
  resultsB$FittedTemporalTrends
  generateRatesGraph(resultsA, resultsB, 'FittedTemporalTrends')
  
  
  # Cross-Sectional Age Curve
  resultsA$CrossAge
  resultsB$CrossAge
  generateRatesGraph(resultsA, resultsB, 'CrossAge')

  
  ########## APC of Rate Ratios ##########
  
  # Fitted Cohort Pattern
  comparison$IO
  
  ggplot(as.data.frame(comparison$FVCA$FCP), aes(Coh)) + 
    geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + 
    geom_line(aes(y = FCP)) + 
    geom_point(aes(y = FCP))
  
  # Fitted Temporal Trends
  ggplot(as.data.frame(comparison$FVPA$FTT), aes(Per)) + geom_line(aes(y = FTT)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + geom_point(aes(y = FTT))
  
  # Cross-Sectional Age Curve
  ggplot(as.data.frame(comparison$FVAP$CAC), aes(Age)) + geom_line(aes(y = CAC)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + geom_point(aes(y = CAC))
  
  # Intercepts
  comparison$IO  

}



generatePlots <- function(csvA, csvB) {
  
  resultsA = apc2(parseJSON(csvToJSON(csvA)))
  resultsB = apc2(parseJSON(csvToJSON(csvB)))
  comparison = rrcomp1(resultsA, resultsB)
  wald = apcwaldtests2(resultsA, resultsB)

  # Rates
  matplot(getRates(csvA), type=c("b"), pch = 1, col = 1:9)
  matplot(getRates(csvB), type=c("b"), pch = 1, col = 1:9)
  
  # Rate Ratios
  corrplot(getRateRatios(csvB, csvA), addCoef.col = "black")
  
  
  ########## APC of Rates ##########
  
  # Local Drifts
  resultsA$LocalDrifts
  resultsB$LocalDrifts
  
  ggplot(as.data.frame(resultsA$LocalDrifts), aes(Age)) + geom_line(aes(y = `Percent per Year`)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1)  + geom_point(aes(y = `Percent per Year`))
  ggplot(as.data.frame(resultsB$LocalDrifts), aes(Age)) + geom_line(aes(y = `Percent per Year`)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1)  + geom_point(aes(y = `Percent per Year`))
  
  
  # Net Drifts
  resultsA$NetDrift
  resultsB$NetDrift
  
  
  # Comparision of Adjusted Rates
  wald$W[14:17,]
  ## Generate Comparison 'Stemp plot
  
  
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
  comparison$IO
  
  ggplot(as.data.frame(comparison$FVCA$FCP), aes(Coh)) + 
    geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + 
    geom_line(aes(y = FCP)) + 
    geom_point(aes(y = FCP))
  
  # Fitted Temporal Trends
  ggplot(as.data.frame(comparison$FVPA$FTT), aes(Per)) + geom_line(aes(y = FTT)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + geom_point(aes(y = FTT))

  # Cross-Sectional Age Curve
  ggplot(as.data.frame(comparison$FVAP$CAC), aes(Age)) + geom_line(aes(y = CAC)) + geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.1) + geom_point(aes(y = CAC))
  
  # Intercepts
  comparison$IO
}


generatePlot <- function(data, key) {
  
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
  
  data
  # for matplot
  #matrix(unlist(data), nrow = ncol(data), byrow = T)
}


#################################################
## Reads Two csv files and returns a matrix of rate ratios
#################################################

getRateRatios <- function(csvFileA, csvFileB) {
  
  a = parseJSON(csvToJSON(csvFileA))
  b = parseJSON(csvToJSON(csvFileB))
  
  (a$offset/a$events) / (b$offset/b$events)
}


parseClientJSON <- function(data) {
  data = fromJSON(data)
  
  titleA = data$titleA
  titleB = data$titleB
  
  tableA = as.data.frame(data$tableA)
  tableB = as.data.frame(data$tableB)
  
  sequenceA = seq_along(tableA) %% 2
  sequenceB = seq_along(tableB) %% 2
  
  endAge    = data$startAge  + data$interval * nrow(tableA)
  endYear   = data$startYear + data$interval * ncol(tableA) / 2
  
  list(
    a = list(
      name         = data$titleA,
      description  = data$description,
      events       = as.matrix(tableA[sequenceA == 1]),
      offset       = as.matrix(tableA[sequenceA == 0]),
      offset_tick  = 100000,
      ages         = seq(data$startAge,  endAge,  by = data$interval),
      periods      = seq(data$startYear, endYear, by = data$interval)
    ),
    b = list(
      name         = data$titleB,
      description  = data$description,
      events       = as.matrix(tableB[sequenceB == 1]),
      offset       = as.matrix(tableB[sequenceB == 0]),
      offset_tick  = 100000,
      ages         = seq(data$startAge,  endAge,  by = data$interval),
      periods      = seq(data$startYear, endYear, by = data$interval)
    )
  )
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


#################################################
## Expected JSON from client
#################################################
clientJSON = '{"titleA":"Urban China", "titleB":"NHW 30-49","description":"C2_urban_pre vs NHW2_pre","startYear":1991,"startAge":30,"interval":2,"tableA":[[73.2066,765207.7647,58.9435,697118.8448,38.8843,638281.2823,45.9227,613253.5343,62.9091,723205.141,69.3862,824882.6295,147.7172,1141010.206,171.6024,1346903.529,156.1185,1360932.631],[115.592,817287.384,110.4479,786195.96,78.5184,708544.896,76.3809,660246.944,94.6749,765263.8,105.605,880547.992,202.096,1206517.44,234.32,1391589.704,243.4695,1368382.664],[168.5383,837240.1724,182.8063,852898.4106,144.4185,756489.776,120.5921,727603.1935,137.5574,851769.6613,157.2572,946301.0182,247.7743,1229975.014,284.9132,1420071.008,377.8658,1501809.788],[216.5463,808142.7884,252.5982,864740.6506,222.6304,775850.88,194.8055,797722.0495,216.4541,946702.0613,242.215,1007239.426,324.0783,1252161.774,383.1932,1455927.304,549.1354,1653627.724],[244.1169,713071.8905,296.4031,789237.134,299.1998,760363.1656,315.2702,853003.2786,356.2624,1014040.336,378.3507,1048460.934,470.3342,1313856.565,588.9712,1522738.456,747.1063,1716250.194],[252.6645,569345.269,316.1415,646118.073,367.1442,711685.0421,459.2421,882654.5914,536.8542,1047729.365,557.4331,1071809.504,686.5247,1406151.685,881.0251,1605613.827,966.8777,1700182.614],[250.5421,449177.2745,323.5682,519953.977,400.9613,637539.7189,532.9181,846567.7405,673.3875,1029813.124,743.4285,1087252.683,965.7418,1486324.361,1164.0645,1640890.027,1190.6163,1666157.304],[240.0518,370342.7305,321.378,431475.0306,394.8145,540922.1263,514.9549,736953.4623,744.5686,956766.3221,923.705,1095753.932,1299.3083,1541783.893,1412.5135,1614186.065,1413.5365,1630327.172],[226.1097,326430.0559,303.9619,371195.0536,361.1,446307.2075,453.8188,605281.1218,748.4794,850754.3979,1014.9194,1062159.449,1527.3665,1528600.132,1586.049,1560803.637,1628.1086,1614998.268],[213.6319,311027.67,265.7107,329627.8658,312.2143,378169.9053,397.9766,503020.084,683.2019,733942.7906,933.7284,951315.432,1490.0587,1402842.929,1644.3479,1516044.443,1826.803,1642476.643]],"tableB":[[222.3326,1356889.76,226.7705,1292792.635,211.4172,1201615.978,201.4584,1095995.083,156.6703,1009392.973,196.1312,980417.9689,198.4267,933429.8686,167.1172,860221.4848,158.8398,840983.1265],[362.368,1391172.536,352.624,1361798.24,336.76,1276974.848,336.7432,1166888.32,285.7149,1082426.496,309.464,1053787.272,301.936,980227.592,259.6938,873950.968,251.056,831994.392],[555.1199,1365201.063,511.2283,1371908.134,505.3999,1324344.634,531.3234,1248173.656,494.7465,1171438.217,453.5141,1103912.732,426.0197,1012513.677,378.3193,932717.7736,385.1749,891001.6025],[791.1519,1317184.927,712.6043,1345289.694,724.0399,1339400.386,773.1787,1311960.936,757.4218,1251144.721,659.6501,1149905.66,613.0837,1053583.685,561.222,1010736.606,568.5189,969699.0105],[1061.0277,1285333.714,966.7729,1304110.297,999.3829,1317817.155,1050.289,1330360.005,1047.3972,1296262.593,959.2405,1210877.367,905.5339,1126733.178,846.6303,1082222.168,808.4104,1019780.868],[1356.3573,1261619.065,1268.1389,1251944.781,1325.9534,1268354.298,1360.9782,1308013.672,1364.881,1307374.714,1337.1976,1276716.84,1291.326,1219602.883,1226.5508,1146107.443,1114.5289,1049208.567],[1645.2217,1207282.39,1592.5797,1199237.295,1680.6834,1226801.407,1700.633,1268343.625,1715.2867,1291209.721,1727.7158,1303656.311,1714.9077,1278704.307,1662.3612,1202608.764,1524.3184,1098228.67],[1916.8255,1113712.829,1930.5561,1148120.503,2054.2061,1200870.43,2065.8072,1217566.143,2097.7166,1250260.67,2112.9098,1282033.491,2158.2115,1291292.775,2139.3239,1251040.415,2040.7088,1175271.576],[2106.4038,1005722.438,2201.3764,1087591.808,2363.7227,1162146.024,2403.7691,1163980.918,2460.3909,1203178.149,2459.7939,1241280.381,2525.1037,1271799.668,2540.5468,1274387.051,2515.7905,1242818.706],[2149.1918,908123.2776,2324.3489,1006648.613,2526.4344,1082212.842,2661.7869,1115887.643,2751.5301,1168612.747,2735.3829,1210828.978,2719.4511,1234656.367,2749.1383,1255633.326,2801.6534,1263351.481]]}'

