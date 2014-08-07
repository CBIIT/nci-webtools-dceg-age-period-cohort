library('RJSONIO')
library('stringr')
source ('./BiomarkerComparison.R')

imageDirectory="./tmp/"  
#######
#
#  example urlEncodedString
#  
#  numberOfValues=7&refSpec=0.7&refSens=0.8&specArray=0.68%2C0.5&specArrayWithRef=0.7%2C0.68%2C0.5&sensArray=0.85%2C0.9&sensArrayWithRef=0.8%2C0.85%2C0.9&labels=2%2C3&unique_key=1407355638719&_=1407355636766
#
#  numberOfValues=8&refSpec=0.7&refSens=0.8&specArray=0.68%2C0.5&specArrayWithRef=0.7%2C0.68%2C0.5&sensArray=0.85%2C0.9&sensArrayWithRef=0.8%2C0.85%2C0.9&prev=0.5&labels=2%2C3&unique_key=1407355874662&_=1407355636767
#
######

getDataJSON <- function(urlEncodedString)
{
  inputList=parseURLEncodedString(urlEncodedString)
  
  numberOfValue <- inputList[[1]][[1]]
  refSpec <- as.numeric(inputList[[2]][[1]])
  refSens <- as.numeric(inputList[[3]][[1]])
  specArray <- getVector((inputList[[4]][[1]]))
  specArrayWithRef <- getVector(inputList[[5]][[1]])
  sensArray <- getVector(inputList[[6]][[1]])
  sensArrayWithRef <- getVector(inputList[[7]][[1]])
  
  # prevalence value defined
  if (numberOfValue==8) {
    prevalence<- getVector(inputList[[8]][[1]])
    labels <- getVector(inputList[[9]][[1]])
    uniqueId <- inputList[[10]][[1]]
    data<-SensSpecLRPV(sensArrayWithRef, specArrayWithRef, prevalence)
  }
  # prevalence value not defined
  else {
    labels <- getVector(inputList[[8]][[1]])
    uniqueId <- inputList[[9]][[1]]
    data<-SensSpecLR(sensArrayWithRef, specArrayWithRef)
  }
  
  graphFile=drawGraph(refSens, refSpec, sensArray, specArray, labels, uniqueId)
  
  jsonString = ""
  jsonString=toJSON(round(data,3), method="C")
}

# plot the graph
drawGraph<-function(refSens, refSpec, sensArrayWithRef, specArrayWithRef, labels, uniqueId) {
   dir.create(imageDirectory)
   graphFile = paste(imageDirectory, "SensSpecLR-", uniqueId, ".png", sep = '')
   png(file = graphFile)
   DrawSensSpecLR(refSens, refSpec, sensArrayWithRef, specArrayWithRef,labels)
   dev.off();
   graphFile;
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

getVector <- function (vectorstring) {
  returnvector<-numeric();
  splitString<-strsplit(vectorstring, ",");
  for (i in splitString) {
    returnvector<-append(returnvector, as.numeric(i));
  };
  return(returnvector);
}