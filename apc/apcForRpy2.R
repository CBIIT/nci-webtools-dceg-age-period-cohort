library('RJSONIO')
library('stringr')
#source ('~/myproject/nci-analysis-tools-web-presence/src/apc/apcLatest.R')
source ('apcLatest.R')

# Use These directories when testing without running Flask Server
#imageDirectory <- "~/myproject/nci-analysis-tools-web-presence/src/apc/static/csv/"; #"static/img";
#csvDirectory <- "~/myproject/nci-analysis-tools-web-presence/src/apc/static/csv/"; #"static/csv";

# Use these directories when using Flask server
imageDirectory <- "static/img/";
csvDirectory <- "static/csv/";

moveRowLabelsToData <- function (matrixWithNamedRows) {
  dataNoRowNames <- data.frame(row.names(matrixWithNamedRows), matrixWithNamedRows, row.names = NULL);
  colnames(dataNoRowNames)[1]<- ' ';
  colnames(dataNoRowNames)[2]<- "Parameter";
  colnames(dataNoRowNames)[3]<- "SD";
  colnames(dataNoRowNames)[4]<- "CI Lo";
  colnames(dataNoRowNames)[5]<- "CI Hi";
  row1 <- list(dataNoRowNames[1,]);
  row2 <- list(dataNoRowNames[2,]);
  row3 <- list(dataNoRowNames[3,]);
  row4 <- list(dataNoRowNames[4,]);
  c(row1,row2, row3, row4);
}

parseURLEncodedString <- function (urlEncodedString) {
  string <- URLdecode(urlEncodedString);
  inputList <- lapply(strsplit(string, "&")[[1]], function(x){
    tmp <- strsplit(x, "=")
    val <- tmp[[1]][[2]]
    names(val) <- tmp[[1]][[1]]
    as.list(val)
  });
}

getKey <- function (inputList) {
  inputList[[1]][[1]];  
}

getTitle <- function (inputList) {
  inputList[[2]][[1]];
}

getStartYear <- function (inputList) {
  inputList[[3]][[1]];
}

getStartAge <- function (inputList) {
  inputList[[4]][[1]];
}

getInterval <- function (inputList) {
  inputList[[5]][[1]];
}

getPeriods <- function (inputList) {
  inputList[[6]][[1]];
}

getRowCount <- function (inputList) {
  inputList[[7]][[1]];
}

getCountData <- function(inputList) {  
  inputList[[8]][[1]];
}

getPopulationData <- function (inputList) {
  inputList[[9]][[1]];
}

getUniqueId <- function(inputList) {
  inputList[[10]][[1]];  
}

createPanCanList <- function (inputList) {
 
  startYear <- as.numeric(getStartYear(inputList));
  startAge <- as.numeric(getStartAge(inputList));
  periods <- as.numeric(getPeriods(inputList));
  rowCount <- as.numeric(getRowCount(inputList));
  countCSV <-getCountData(inputList);
  populationCSV <- getPopulationData(inputList);
                         
  interval <- switch(getInterval(inputList), year1 = 1, year2 = 2, year3 = 3, year4 = 4, year4 = 5);
  panCan <- list(name = getTitle(inputList),
                 # events = to Count Columns in APC Spread Sheet
                 events =  matrix(c(as.numeric(strsplit(countCSV,',')[[1]])), ncol = periods, nrow = rowCount, byrow=TRUE), 
                 # offset = to Population Columns in APC Spread Sheet
                 offset = matrix(c(as.numeric(strsplit(populationCSV,',')[[1]])), ncol = periods, nrow = rowCount, byrow=TRUE), 
                 offset_tick = 100000,
                 # age one more than total Rows,
                 ages = seq(startAge, ((rowCount*interval)+(startAge)), by = interval), 
                 # periods one more than tot Col
                 periods = seq(startYear, ((periods*interval)+(startYear)), by = interval) 
  );
  panCan;
}

getGraph <- function (apcOutput, keyGraphName, uniqueId) {
  png(file =  paste(imageDirectory, keyGraphName, uniqueId, ".png", sep = ''), units="in", width=10, height=8, res=150);
  line.apc(apcOutput, keyGraphName);
  dev.off();
}

getApcData <- function (urlEncodedString) {
  apcdata<-apc(createPanCanList (urlEncodedString));
}

getApcDataJSON <-function(urlEncodedString)
{
  inputList <- parseURLEncodedString(urlEncodedString);
  apcdata<-getApcData (inputList);
  
  jsonString = "";
  key <- getKey(inputList);
  uniqueId <- getUniqueId(inputList);
  
  if (key == "AgeDeviations") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$AgeDeviations)
    jsonString <- toJSON(apcdata$AgeDeviations, method="C");
  } else if (key == "PerDeviations") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$PerDeviations)
    jsonString <- toJSON(apcdata$PerDeviations, method="C");
  } else if (key == "CohDeviations") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$CohDeviations)
    jsonString <- toJSON(apcdata$CohDeviations, method="C");
  } else if (key == "LongAge") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$LongAge)
    jsonString <- toJSON(apcdata$LongAge, method="C");
  } else if (key == "CrossAge") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$CrossAge)
    jsonString <- toJSON(apcdata$CrossAge, method="C");
  } else if (key == "Long2CrossRR") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$Long2CrossRR)
    jsonString <- toJSON(apcdata$Long2CrossRR, method="C");
  } else if (key == "FittedTemporalTrends") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$FittedTemporalTrends)
    jsonString <- toJSON(apcdata$FittedTemporalTrends, method="C");
  } else if (key == "PeriodRR") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$PeriodRR)
    jsonString <- toJSON(apcdata$PeriodRR, method="C");
  } else if (key == "CohortRR") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$CohortRR)
    jsonString <- toJSON(apcdata$CohortRR, method="C");
  } else if (key == "LocalDrifts") {
    getGraph(apcdata, key, uniqueId);
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$LocalDrifts)
    jsonString <- toJSON(apcdata$LocalDrifts, method="C");
    print(apcdata$LocalDrifts);
  } else if (key == "Coefficients") {
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), x=apcdata$Coefficients)
    coefficients <- moveRowLabelsToData(apcdata$Coefficients);
    jsonString <- toJSON(coefficients, method="C", );
    print(coefficients);
  } else if (key == "Waldtests") {
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$Waldtests)
    jsonString <- toJSON(apcdata$Waldtests, method="C");
  } else if (key == "NetDrift") {
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$NetDrift)
    jsonString <- toJSON(apcdata$NetDrift, method="C");
  } else if (key =="Offset") {
    write.csv(file=paste(csvDirectory, key, uniqueId, ".csv", sep = ''), row.names=FALSE, x=apcdata$FittedRates$offset_tick)
    jsonString <- toJSON(apcdata$FittedRates$offset_tick, method="C");
  }else {
    jsonString <- toJSON(apcdata, method="C");
  }
  str_replace_all(jsonString, "[\n]","");
}

# Call from Python....
#getApcDataJSON("key=LocalDrifts&title=Female%2520Pancreas%2520Cancer&startYear=1993&startAge=33&interval=year4&periodCount=4&rowCount=14&countCSV=461%2C520%2C559%2C627%2C1065%2C1217%2C1323%2C1512%2C2160%2C2554%2C2707%2C3000%2C3495%2C4163%2C4927%2C5237%2C4785%2C5559%2C6490%2C7559%2C5106%2C6358%2C6824%2C7980%2C4873%2C6715%2C7038%2C8156%2C4863%2C6460%2C7244%2C9161%2C5237%2C6176%2C6531%2C9068%2C5628%2C5983%2C5871%2C7951%2C6060%2C6089%2C5520%2C6627%2C5722%2C6099%2C5586%2C5822%2C4185%2C4909%2C5053%2C5273%2C5227%2C6077%2C6681%2C7977%2C&populationCSV=5059030%2C4881390%2C4592280%2C4522470%2C5235420%2C4974730%2C4803820%2C4594620%2C5009550%2C5121610%2C4885700%2C4747800%2C4568930%2C4932280%2C5016980%2C4796880%2C4069220%2C4496530%2C4834800%2C4943780%2C3363020%2C4088640%2C4403470%2C4758850%2C2748540%2C3288620%2C3966000%2C4324500%2C2277930%2C2602620%2C3136830%2C3819230%2C2100130%2C2174230%2C2466900%2C3011310%2C2078240%2C1971090%2C2051330%2C2345100%2C1955640%2C1912020%2C1838950%2C1920210%2C1762440%2C1805430%2C1730210%2C1664750%2C1377240%2C1509560%2C1561980%2C1511610%2C2147130%2C2343580%2C2580260%2C2826770%2C&uniqueId=12344343334dska3"); # Does not produce graph#
#getApcDataJSON("key=Coefficients&title=Female%2520Pancreas%2520Cancer&startYear=1993&startAge=33&interval=year4&periodCount=4&rowCount=14&countCSV=461%2C520%2C559%2C627%2C1065%2C1217%2C1323%2C1512%2C2160%2C2554%2C2707%2C3000%2C3495%2C4163%2C4927%2C5237%2C4785%2C5559%2C6490%2C7559%2C5106%2C6358%2C6824%2C7980%2C4873%2C6715%2C7038%2C8156%2C4863%2C6460%2C7244%2C9161%2C5237%2C6176%2C6531%2C9068%2C5628%2C5983%2C5871%2C7951%2C6060%2C6089%2C5520%2C6627%2C5722%2C6099%2C5586%2C5822%2C4185%2C4909%2C5053%2C5273%2C5227%2C6077%2C6681%2C7977%2C&populationCSV=5059030%2C4881390%2C4592280%2C4522470%2C5235420%2C4974730%2C4803820%2C4594620%2C5009550%2C5121610%2C4885700%2C4747800%2C4568930%2C4932280%2C5016980%2C4796880%2C4069220%2C4496530%2C4834800%2C4943780%2C3363020%2C4088640%2C4403470%2C4758850%2C2748540%2C3288620%2C3966000%2C4324500%2C2277930%2C2602620%2C3136830%2C3819230%2C2100130%2C2174230%2C2466900%2C3011310%2C2078240%2C1971090%2C2051330%2C2345100%2C1955640%2C1912020%2C1838950%2C1920210%2C1762440%2C1805430%2C1730210%2C1664750%2C1377240%2C1509560%2C1561980%2C1511610%2C2147130%2C2343580%2C2580260%2C2826770%2C&uniqueId=12344343334dska3"); # Does not produce graph
