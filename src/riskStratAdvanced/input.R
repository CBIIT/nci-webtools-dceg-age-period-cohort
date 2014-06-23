library('RJSONIO')
library('stringr')
source ('riskStratAdvancedOptionWrapper.R')

imageDirectory <- "./img/";
rdirectory <- "";
drawfunctionprefix = "Draw"
keysforfunction = list("Sens"=1, "Spec"=2, "PPV"=3, "cNPV"=4, "Prev"=5, "Delta"=6);
#keysforfunction = list("1"="Sens", "2"="Spec", "3"="PPV", "4"="cNPV", "5"="Prev", "6"="Delta");
functionnames = list("sensitivity"="Sens", "specificity"="Spec", "ppv"="PPV", "cnpv"="cNPV", "prevalence"="Prev", "delta"="Delta");
rfunctionname = ""

inputtransposelist = list();
datatransposed = "";

getDataJSON <-function(urlEncodedString)
{
  dir.create(imageDirectory);
  inputList <- parseURLEncodedString(urlEncodedString);
  independent<-getindependent(inputList);   #PPV -2
  independentvalue<-getindependentValue(inputList);
  independentVector<-getVector(getindependentValue(inputList));
  fixed<-getfixed(inputList);   #PREV -3
  fixedvalue<-getfixedValue(inputList);
  fixedVector<-getVector(getfixedValue(inputList));
  contour<- getcontour(inputList);   #SPEC -1
  contourvalue<-getcontourValue(inputList);
  contourVector<-getVector(getcontourValue(inputList));
  key<-getKey(inputList);
  keynumber<-getKeyNumber(inputList);
  specmin<-getSpecMin(inputList);
  specmax<-getSpecMax(inputList);
  uniqueId <- getUniqueId(inputList);
  tab <- gettab(inputList);
  tabvalue <- gettabvalue(inputList);
  keyGraphName <- getgraphname(inputList);
  jsonString = "";
  json_string = "";
  errorString = "";
  assign("last.warning", NULL, envir = baseenv());
  result<-try(getTable(independentvalue, fixedvalue, contourvalue, independent, fixed, contour, gsub("\n","",keyGraphName), keynumber, tabvalue, uniqueId));

  resultCheck = is(result,"try-error");

  if (resultCheck == "FALSE") {
    json_string = paste("[{\"error\": [{ \"errortrue\": 0}, {\"message\": \"", " ", "\"}], \"data\":", str_replace_all(toJSON(result[,,as.numeric(tab)], method="C"), "[\n]",""), "}]")
  } else {
    json_string = paste("[{\"error\": [{ \"errortrue\": 1}, {\"message\": \"",  str_replace_all(result[1], "[\n]",""), "\"}], \"data\":{}}]")
  }

  print(json_string);
  return (json_string);
}

getVector <- function (vectorstring) {
  returnvector<-numeric();
  splitString<-strsplit(vectorstring, ",");
  for (i in splitString) {
    returnvector<-append(returnvector, as.numeric(i));
  };
  return(returnvector);
}



getDrawFunctionName <- function (drawfunctionprefix, key, rfunctionname) {
  rDrawFileName = paste(rdirectory);
  rDrawFileName = paste(rDrawFileName, drawfunctionprefix, functionnames[[tolower(c(key))]], rfunctionname, sep = "")
  
  print(rDrawFileName)
  print(rfunctionname)
  return(rDrawFileName)
}


getDeltaSpecGraph <- function (specmin, specmax, prev, ppv, keyGraphName, uniqueId) {
  imageFileName = paste(imageDirectory, keyGraphName, uniqueId, "-", prev, ".png", sep = '');
  png(file = imageFileName , units="in", width=10, height=8, res=150);
  DrawDeltaSpecPPV(specmin, specmax, prev, ppv);
  dev.off();
  imageFileName;
}

getSensSpecGraph <- function (specmin, specmax, prev, ppv, keyGraphName, uniqueId) {
  imageFileName = paste(imageDirectory, keyGraphName, uniqueId, "-", prev, ".png", sep = '');
  png(file = imageFileName , units="in", width=10, height=8, res=150);
  DrawSensSpecPPV(specmin,specmax,prev, ppv);
  dev.off();
  imageFileName;
}

getKey <- function (inputList) {
  inputList[[1]][[1]];
}

getKeyNumber <- function (inputList){
  inputList[[2]][[1]];
  
}

getindependent <- function (inputList) {
  inputList[[3]][[1]];
}

getfixed <- function (inputList) {
  inputList[[4]][[1]];
}

getcontour <- function (inputList) {
  inputList[[5]][[1]];
}

getindependentValue <- function (inputList) {
  inputList[[6]][[1]];
}

getfixedValue <- function (inputList) {
  inputList[[7]][[1]];
}

getcontourValue <- function (inputList) {
  inputList[[8]][[1]];
}

getSpecMin <- function (inputList) {
  inputList[[9]][[1]];
}

getSpecMax <- function (inputList) {
  inputList[[10]][[1]];
}

getUniqueId <- function (inputList) {
  inputList[[11]][[1]];
}

gettab <- function (inputList) {
  inputList[12][[1]];
}

gettabvalue <- function (inputList) {
  inputList[[13]][[1]];
}

getgraphname <- function (inputList) {
  inputList[[14]][[1]];
}

JsonWrapper <- function(dppv,prev,spec)
{
  data<- PPVPrevSpec(dppv,prev,spec);
  jsonString = "";
  jsonString=toJSON(round(data,3), method="C");
  str_replace_all(jsonString, "[\n]","");
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
