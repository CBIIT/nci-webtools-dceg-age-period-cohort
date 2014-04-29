library('RJSONIO')
library('stringr')
source ('./r-code/PPVPrevSpec.R')
source ('./r-code/DrawDeltaSpecPPV.R')
source ("./r-code/DrawSensSpecPPV.R")

imageDirectory <- "../img";

getDataJSON <-function(urlEncodedString)
{
  inputList <- parseURLEncodedString(urlEncodedString);
  contour<-getContour(inputList);
  dppv<-getVector(contour);
  fixed<-getFixedValue(inputList);
  prev<-getVector(fixed);
  dependent<-getDependent(inputList);
  spec<-getVector(dependent);
  key<-getKey(inputList);
  key<-getKey(inputList);
  specmin<-getSpecMin(inputList);
  specmax<-getSpecMax(inputList);
  uniqueId <- getUniqueId(inputList);
  tab <- gettab(inputList);
  tabvalue <- gettabvalue(inputList);
  keyGraphName <- getgraphname(inputList);
  data<- PPVPrevSpec(dppv,prev,spec);
  jsonString = "";

  if (key == "Sensitivity_required_to_achieve_specified_PPV_given_prevalence_and_specificity"){
    jsonString=toJSON(data$"Sensitivity_required_to_achieve_specified_PPV_given_prevalence_and_specificity"[,,as.numeric(tab)], method="C");
    #getDeltaSpecGraph(as.numeric(specmin), as.numeric(specmax), as.numeric(tabvalue), ppv, keyGraphName, uniqueId);
  } else if (key == "Delta_required_to_achieve_specified_PPV_given_prevalence_and_specificity") {
    jsonString=toJSON(data$"Delta_required_to_achieve_specified_PPV_given_prevalence_and_specificity"[,,as.numeric(tab)], method="C");
    #getSensSpecGraph(as.numeric(specmin), as.numeric(specmax), as.numeric(tabvalue), ppv, keyGraphName, uniqueId);
  } else if (key == "Prevalence_Odds_Length") {
    jsonString=toJSON(length(data$"Prevalence_Odds"), method="C");
  }
  str_replace_all(jsonString, "[\n]","");
}

getVector <- function (vectorstring) {
   returnvector<-numeric();
   splitString<-strsplit(vectorstring, ",");
   for (i in splitString) {
     returnvector<-append(returnvector, as.numeric(i));
   };
   return(returnvector);
}

getDeltaSpecGraph <- function (specmin, specmax, prev, ppv, keyGraphName, uniqueId) {
  imageFileName = paste(imageDirectory, keyGraphName, uniqueId, ".png", sep = '');
  png(file = imageFileName , units="in", width=10, height=8, res=150);
  DrawDeltaSpecPPV(specmin, specmax, prev, ppv);
  dev.off();
  imageFileName;
}

getSensSpecGraph <- function (specmin, specmax, prev, ppv, keyGraphName, uniqueId) {
  imageFileName = paste(imageDirectory, keyGraphName, uniqueId, ".png", sep = '');
  png(file = imageFileName , units="in", width=10, height=8, res=150);
  DrawSensSpecPPV(specmin,specmax,prev, ppv);
  dev.off();
  imageFileName;
}

getKey <- function (inputList) {
  inputList[[1]][[1]];
}

getContour <- function (inputList) {
  inputList[[2]][[1]];
}

getFixedValue <- function (inputList) {
  inputList[[3]][[1]];
}

getDependent <- function (inputList) {
  inputList[[4]][[1]];
}

getSpecMin <- function (inputList) {
  inputList[[5]][[1]];
}

getSpecMax <- function (inputList) {
  inputList[[6]][[1]];
}

getUniqueId <- function (inputList) {
  inputList[[7]][[1]];
}

gettab <- function (inputList) {
  inputList[[8]][[1]];
}

gettabvalue <- function (inputList) {
  inputList[[9]][[1]];
}

getgraphname <- function (inputList) {
  inputList[[10]][[1]];
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

