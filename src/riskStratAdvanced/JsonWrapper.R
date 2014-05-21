library('RJSONIO')
library('stringr')
source ('RiskStratAnalysisOptions.R')

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
  specmin<-getSpecMin(inputList);
  specmax<-getSpecMax(inputList);
  uniqueId <- getUniqueId(inputList);
  tab <- gettab(inputList);
  tabvalue <- gettabvalue(inputList);
  keyGraphName <- getgraphname(inputList);
  #data<- PPVPrevSpec(dppv,prev,spec);
  rfunctionname = getFunctionName(independent, fixed, contour);
  rfunction <- get(rfunctionname, mode="function")
  #data<- functionToCall(independentVector,fixedVector,contourVector);
  functionvalueorder <- getfunctionvalueorder(getindependent(inputList), getfixed(inputList), getcontour(inputList), independentvalue, fixedvalue, contourvalue);
  firstinputVector<-getVector(functionvalueorder[[1]]);
  secondinputVector<-getVector(functionvalueorder[[2]]);
  thirdinputVector<-getVector(functionvalueorder[[3]]);
  data<- rfunction(firstinputVector,secondinputVector,thirdinputVector);
  tranposeorder = "";
  tranposeorder <- gettransposeorder(getindependent(inputList), getfixed(inputList), getcontour(inputList));
  jsonString = "";

  # can be refactored to iterate through keys- table titles with the title mapping both in R and javascript
  if (key == "Sensitivity"){
    datatransposed <- getTransposedData(getindependent(inputList), getfixed(inputList), getcontour(inputList), tranposeorder, data$"Sensitivity");
    jsonString=toJSON(datatransposed[,,as.numeric(tab)], method="C");
    drawfunctionname=getDrawFunctionName(drawfunctionprefix, key, rfunctionname);
    drawfunction <- get(drawfunctionname, mode="function");
    imageFileName = paste(imageDirectory, keyGraphName, uniqueId, "-", as.numeric(tabvalue), ".png", sep = '');
    png(file = imageFileName , units="in", width=10, height=8, res=150);
    returnvalue<- drawfunction(firstinputVector,secondinputVector,as.numeric(tabvalue));
    #DrawDeltaSpecPPV(specmin, specmax, prev, ppv);
    dev.off();
    imageFileName;
    
    #getSensSpecGraph(as.numeric(specmin), as.numeric(specmax), as.numeric(tabvalue), dppv, keyGraphName, uniqueId);
  } else if (key == "Delta") {
    datatransposed <- getTransposedData(getindependent(inputList), getfixed(inputList), getcontour(inputList), tranposeorder, data$"Delta");
    jsonString=toJSON(datatransposed[,,as.numeric(tab)], method="C");
    drawfunctionname=getDrawFunctionName(drawfunctionprefix, key, rfunctionname);
    drawfunction <- get(drawfunctionname, mode="function");
    imageFileName = paste(imageDirectory, keyGraphName, uniqueId, "-", as.numeric(tabvalue), ".png", sep = '');
    png(file = imageFileName , units="in", width=10, height=8, res=150);
    returnvalue<- drawfunction(firstinputVector,secondinputVector,as.numeric(tabvalue));
    #DrawDeltaSpecPPV(specmin, specmax, prev, ppv);
    dev.off();
    imageFileName;
    #getDeltaSpecGraph(as.numeric(specmin), as.numeric(specmax), as.numeric(tabvalue), dppv, keyGraphName, uniqueId);
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

getFunctionName <- function (i, j, k) {
  tranposelist = list();
  rFileName = paste(rdirectory);
  inputnames = list("independent"=i, "fixed"=j, "contour"=k)
  for (name in names(functionnames))
  {
    for (variablename in names(inputnames))
    {
        print(inputnames[[variablename]])
        if (name == inputnames[[variablename]])
        {
           rFileName = paste(rFileName, functionnames[[name]], sep = "")
           #tranposelist = append(name, tranposelist)
           tranposelist <- c(tranposelist, name)
        }
    }
  }
  print(rFileName)
  print(tranposelist)
  return(rFileName)
}

getDrawFunctionName <- function (drawfunctionprefix, key, rfunctionname) {
  rDrawFileName = paste(rdirectory);
  rDrawFileName = paste(rDrawFileName, drawfunctionprefix, functionnames[[tolower(c(key))]], rfunctionname, sep = "")

  print(rDrawFileName)
  print(rfunctionname)
  return(rDrawFileName)
}

gettransposeorder <- function (independent, fixed, contour) {
  tranposelist = list();
  inputnames = list("independent"=independent, "fixed"=fixed, "contour"=contour)
  for (name in names(keysforfunction))
  {
    for (variablename in names(inputnames))
    {
      if (name == functionnames[[inputnames[[variablename]]]])
      {
        tranposelist <- c(tranposelist, name)
      }
    }
  }
  print(tranposelist)
  return(tranposelist)
}

getfunctionvalueorder <- function (independent, fixed, contour, independentvalue, fixedvalue, contourvalue)
{
  tranposevaluelist = list();
  tranposelist = list();
  inputnames = list("independent"=independent, "fixed"=fixed, "contour"=contour)
  inputvalues = list("independent"=independentvalue, "fixed"=fixedvalue, "contour"=contourvalue)
  for (name in names(keysforfunction))
  {
    for (variablename in names(inputnames))
    {
      if (name == functionnames[[inputnames[[variablename]]]])
      {
        tranposevaluelist <- c(tranposevaluelist, inputvalues[[variablename]])
        tranposelist <- c(tranposelist, name)
      }
    }
  }
  print(tranposevaluelist)
  print(tranposelist)
  return(tranposevaluelist)
}

getTransposedData <- function (independent, fixed, contour, tranposeorder, dataMatrix)
{
  dataInputOrder = list("independent"=independent, "fixed"=fixed, "contour"=contour)
  for (i in 1:3)
  {
    for (variablename in names(dataInputOrder))
    {
        if (tranposeorder[[i]] == functionnames[[dataInputOrder[[variablename]]]])
              inputtransposelist[[variablename]] <- i
    }
  }
  print(inputtransposelist[["independent"]])
  print(inputtransposelist[["contour"]])
  print(inputtransposelist[["fixed"]])
  datax <- aperm(dataMatrix, c(inputtransposelist[["independent"]],inputtransposelist[["contour"]],inputtransposelist[["fixed"]]))
  return (datax)
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

getindependent <- function (inputList) {
  inputList[[2]][[1]];
}

getfixed <- function (inputList) {
  inputList[[3]][[1]];
}

getcontour <- function (inputList) {
  inputList[[4]][[1]];
}

getindependentValue <- function (inputList) {
  inputList[[5]][[1]];
}

getfixedValue <- function (inputList) {
  inputList[[6]][[1]];
}

getcontourValue <- function (inputList) {
  inputList[[7]][[1]];
}

getSpecMin <- function (inputList) {
  inputList[[8]][[1]];
}

getSpecMax <- function (inputList) {
  inputList[[9]][[1]];
}

getUniqueId <- function (inputList) {
  inputList[[10]][[1]];
}

gettab <- function (inputList) {
  inputList[11][[1]];
}

gettabvalue <- function (inputList) {
  inputList[[12]][[1]];
}

getgraphname <- function (inputList) {
  inputList[[13]][[1]];
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

