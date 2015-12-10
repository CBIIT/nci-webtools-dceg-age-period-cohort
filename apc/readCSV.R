library(stringr)

#################################################
## Reads csv file and returns contents as a list
## eg: results = apc2(readCSV('NHW.csv'))
#################################################

readCSV <- function(csvFile) {
  
  headers     = read.csv(csvFile, nrow = 5, header = F, stringsAsFactors = F)[[1]]
  data        = read.csv(csvFile, skip = 5, header = F)

  startYear   = as.numeric(parseHeader(headers[3]))
  startAge    = as.numeric(parseHeader(headers[4]))
  interval    = as.numeric(parseHeader(headers[5]))
  range       = interval * nrow(data)
  sequence    = seq_along(data) %% 2
  
  contents = list(
    name         = parseHeader(headers[1]),
    description  = parseHeader(headers[2]),
    events       = as.matrix(data[sequence == 1]), # odd numbered columns
    offset       = as.matrix(data[sequence == 0]), # even numbered columns
    offset_tick  = 100000,
    ages         = seq(startAge, startAge + range,  by = interval),
    periods      = seq(startYear, startYear + range, by = interval)
  );  

  return (contents)
}

parseHeader <- function(header) {
  # splits string on first instance of colon and returns second half
  str_trim(regmatches(header, regexpr(":", header), invert = T)[[1]][2])
}
