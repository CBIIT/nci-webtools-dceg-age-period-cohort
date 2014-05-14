library('RJSONIO')
library('stringr')
source ('./RiskFromROC.R')

imageDirectory <- "./img/";

getDataJSON <-function(urlEncodedString)
{
  #create img directory if it is not there
  dir.create(imageDirectory);
  inputList <- parseURLEncodedString(urlEncodedString);
  #data <- read.csv("p16-ELISA-sample-data.csv")
  option <- getOption(inputList);
  #spec<-c(0.8, 0.9, 0.95, 0.99, 0.995)
  #prev<-c(0.5, 0.6, 0.7, 0.8, 0.9)
  spec <- getVector(getSpec(inputList));
  prev <- getVector(getPrev(inputList));
  jsonString = "";
  if (as.numeric(option)==1)
  {
    #data <- read.csv("p16-ELISA-sample-data.csv")
    data =  matrix(c(as.numeric(strsplit(inputList[[8]][[1]],',')[[1]])), ncol = as.numeric(getcolcount(inputList)), nrow = as.numeric(getrowcount(inputList)), byrow=TRUE);
    risk1<-RiskFromROC(data, spec, prev);
    jsonString=toJSON(risk1, method="C");
    uniqueId <- inputList[[6]][[1]];
    graphname <- inputList[[7]][[1]];
    getRawROCGraph(data, uniqueId, graphname);
  }
  else if (as.numeric(option)==2)
  {
    #cases<-c(4, 0.1, 100)
    #controls<-c(1, 0.1, 200)
    cases <- getVector(inputList[[4]][[1]]);
    controls <- getVector(inputList[[5]][[1]]);
    risk2<-deltaspecppv(cases, controls, spec, prev);
    specmin<-min(spec);
    specmax<-max(spec);
    jsonString=toJSON(risk2, method="C");
    uniqueId <- inputList[[6]][[1]];
    graphname <- inputList[[7]][[1]];
    getROCGraph(specmin, specmax, risk2$Delta[8,3], uniqueId, graphname);
  }

  str_replace_all(jsonString, "[\n]","");
}

getData <-function(urlEncodedString)
{
  inputList <- parseURLEncodedString(urlEncodedString);
  data <- read.csv("p16-ELISA-sample-data.csv")
  spec<-c(0.8, 0.9, 0.95, 0.99, 0.995)
  prev<-c(0.5, 0.6, 0.7, 0.8, 0.9)
  source("RiskFromROC.R")
  risk1<-RiskFromROC(data, spec, prev)
  DrawRawROC(data)
}

getVector <- function (vectorstring) {
  returnvector<-numeric();
  splitString<-strsplit(vectorstring, ",");
  for (i in splitString) {
    returnvector<-append(returnvector, as.numeric(i));
  };
  return(returnvector);
}

parseURLEncodedString <- function (urlEncodedString) {
  #print (urlEncodedString);
  string <- URLdecode(urlEncodedString);
  inputList <- lapply(strsplit(string, "&")[[1]], function(x){
    tmp <- strsplit(x, "=")
    val <- tmp[[1]][[2]]
    names(val) <- tmp[[1]][[1]]
    as.list(val)
  });
}

getRawROCGraph <- function (data, uniqueId, keyGraphName) {
  imageFileName = paste(imageDirectory, keyGraphName, uniqueId, ".png", sep = '');
  png(file = imageFileName , units="in", width=10, height=8, res=150);
  DrawRawROC(data);
  dev.off();
  imageFileName;
}

getROCGraph <- function (specmin, specmax, delta, uniqueId, keyGraphName) {
  imageFileName = paste(imageDirectory, keyGraphName, uniqueId, ".png", sep = '');
  png(file = imageFileName , units="in", width=10, height=8, res=150);
  DrawROC(specmin, specmax, delta);
  dev.off();
  imageFileName;
}

getOption <- function (inputList) {
  inputList[[1]][[1]];
}

getSpec <- function (inputList) {
  inputList[[2]][[1]];
}

getPrev <- function (inputList) {
  inputList[[3]][[1]];
}

getrowcount <- function (inputList) {
  inputList[[4]][[1]];
}

getcolcount <- function (inputList) {
  inputList[[5]][[1]];
}

getdata <- function (inputList) {
  inputList[[6]][[1]];
}


urlEncodedString<- "option=1&spec=0.8%2c0.9%2c0.95%2c0.99%2c0.995&prev=0.5%2c0.6%2c0.7%2c0.8%2c0.9&cases=4%2c0.1%2c100&controls=1%2c0.1%2c200"
urlEncodedString<- "option=2&spec=0&prev=0&datarowcount=9&colcount=2&dataCSV=p16_ELISA%2cHGCIN%2c0.00%2c0%2c0.00%2c0%2c0.00%2c0%2c6.06%2c0%2c6.16%2c0%2c6.3398%2c0%2c6.47%2c0%2c6.81%2c0"

urlEncodedString<- "spec=0.8%2c0.9%2c0.95%2c0.99%2c0.995&prev=0.5%2c0.6%2c0.7%2c0.8%2c0.9&datarowcount=9&colcount=2&dataCSV=0.00%2c0%2c0.00%2c0%2c0.00%2c0%2c6.06%2c0%2c6.16%2c0%2c6.3398%2c0%2c6.47%2c0%2c6.81%2c0"
