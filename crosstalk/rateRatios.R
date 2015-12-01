# read csv files and display a correlation plot

library(stringr)
library(corrplot)

testPlot <- function () {
  
  
  data = generateRR('NHW.csv', 'Urban China.csv')
  
  
  col1 <- colorRampPalette(c("cyan", "#007FFF", "blue", "#871414", "red", "#FF7F00", "yellow", "white" ))
  
  png(height = 1000, width = 1000, pointsize = 15, file = "rplot.png")
  corrplot(data, method = "circle",
           addCoef.col = "black",
           
           tl.col="black", tl.srt=45,
           cl.lim = c(0, 1),
           col = col1(100))
  
  dev.off()
}

#################################################
## Reads Two csv files and returns a matrix of ratios
#################################################

generateRR <- function (csvFileA, csvFileB) {
  
  a = getRates(csvFileA)
  b = getRates(csvFileB)
  data = a/b
  
  contents = readCSV(csvFileA)
  
  year = contents$startYear
  age = contents$startAge
  interval = contents$interval
  
  columns = seq(year, year + interval*(ncol(a)-1), interval)
  
  rows = seq(age, age + interval*(nrow(a)-1), interval)
  
  colnames(data) = paste(columns, columns + interval - 1, sep = " - ")
  rownames(data) = paste(rows, rows + interval - 1, sep = " - ")
  
  return (as.matrix(as.data.frame(data)))
}

getRates <- function (csvFile) {

  data = read.csv(csvFile, skip = 5, header = F)
  rates = matrix()
  
  for (i in seq(1, ncol(data), 2)) {
    rates = cbind(rates, data[i+1]/data[i])
  }
  
  return (rates[2:ncol(rates)])
}


#################################################
## Reads csv file and returns contents as a list
#################################################

readCSV <- function(csvFile) {
  
  
  headers     = read.csv(csvFile, nrow = 5, header = F, stringsAsFactors = F)[[1]]
  data        = read.csv(csvFile, skip = 5, header = F)
  contents    = list()
  
  contents$title       = parseHeader(headers[1])
  contents$description = parseHeader(headers[2])
  contents$startYear   = as.numeric(parseHeader(headers[3]))
  contents$startAge    = as.numeric(parseHeader(headers[4]))
  contents$interval    = as.numeric(parseHeader(headers[5]))
  contents$data        = data
  
  return (contents)
}

parseHeader <- function(header) {
  # splits string on first instance of colon and returns second half
  str_trim(regmatches(header, regexpr(":", header), invert = T)[[1]][2])
}
