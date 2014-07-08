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



calculate <-function(independentVector, fixedVector, contourVector)
{
  independent = deparse(substitute(independentVector));
  fixed = deparse(substitute(fixedVector));
  contour = deparse(substitute(contourVector));
  rfunctionname = getFunctionName(independent, fixed, contour);
  print("+++++++++++")
  print(rfunctionname)
  rfunction <- get(rfunctionname, mode="function")
  functionvalueorder <- getfunctionvalueorder(independentVector, fixedVector, contourVector, independent, fixed, contour);
  print("functionvalueorder")
  print(functionvalueorder)
  print("done functionvalueorder")
  firstinputVector<-getVector(functionvalueorder[[1]]);
  secondinputVector<-getVector(functionvalueorder[[2]]);
  thirdinputVector<-getVector(functionvalueorder[[3]]);
  print(firstinputVector)
  print(secondinputVector)
  print(thirdinputVector)
  data<- rfunction(firstinputVector,secondinputVector,thirdinputVector);
  return (data);
}

calculate <-function(firstinputVector,secondinputVector,thirdinputVector,  independent, fixed, contour)
{
  rfunctionname = getFunctionName(independent, fixed, contour);
  print("+++++++++++")
  print(rfunctionname)
  rfunction <- get(rfunctionname, mode="function")
  print(firstinputVector)
  print(secondinputVector)
  print(thirdinputVector)
  data<- rfunction(firstinputVector,secondinputVector,thirdinputVector);
  return (data);
}


getFunctionName <- function (independent, fixed, contour) {
  tranposelist = list();
  rFileName = paste(rdirectory);
  print(independent)
  print(fixed)
  print(contour)
  inputnames = list("independent"=independent, "fixed"=fixed, "contour"=contour)
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

getfunctionvalueorder <- function (independentVector, fixedVector, contourVector, independent, fixed, contour)
{
  tranposevaluelist = list();
  tranposelist = list();
  independentvalue = independentVector;
  print(independentvalue)
  fixedvalue = fixedVector;
  print(fixedvalue)
  contourvalue = contourVector;
  inputnames = list("independent"=independent, "fixed"=fixed, "contour"=contour)
  inputvalues = list("independent"=independentvalue, "fixed"=fixedvalue, "contour"=contourvalue)
  for (name in names(keysforfunction))
  {
    for (variablename in names(inputnames))
    {
      print(inputnames[[variablename]]);
      if (name == functionnames[[inputnames[[variablename]]]])
      {
        print(tranposevaluelist)
        tranposevaluelist <- c(tranposevaluelist, inputvalues[[variablename]])
        tranposelist <- c(tranposelist, name)
      }
    }
  }
  return(tranposevaluelist)
}


getTable <-function(independentVector, fixedVector, contourVector, key, keynumber, tabvalue, uniqueId)
{
  independent = deparse(substitute(independentVector));
  fixed = deparse(substitute(fixedVector));
  contour = deparse(substitute(contourVector));
  independentvalue = deparse(independentVector);
  print(independentvalue)
  fixedvalue = deparse(fixedVector);
  print(fixedvalue)
  contourvalue = deparse(contourVector);
  return (getTable(independentvalue, fixedvalue, contourvalue, independent, fixed, contour, key, keynumber, tabvalue, uniqueId));
}

getTable <-function(independentValues, fixedValues, contourValues, independent, fixed, contour, key, keynumber, tabvalue, uniqueId)
{
  tranposeorder = "";
  tranposeorder <- gettransposeorder(independent, fixed, contour);
  functionvalueorder <- getfunctionvalueorder(independentValues, fixedValues, contourValues, independent, fixed, contour);
  firstinputVector<-getVector(functionvalueorder[[1]]);
  secondinputVector<-getVector(functionvalueorder[[2]]);
  thirdinputVector<-getVector(functionvalueorder[[3]]);
  data <- calculate(firstinputVector, secondinputVector, thirdinputVector, independent, fixed, contour);

  print(tranposeorder)
  print(keynumber)
  print(data)
  datatransposed <- getTransposedData(independent, fixed, contour, tranposeorder, data[[as.numeric(keynumber)]]);
  print(datatransposed)

  #getSensSpecGraph(as.numeric(specmin), as.numeric(specmax), as.numeric(tabvalue), dppv, keyGraphName, uniqueId);
  drawGraph(independent, fixed, contour, getVector(independentValues), getVector(contourValues), key, tabvalue, uniqueId);
  return (datatransposed);
}

getFunctionNameAsIs <- function (independent, contour, fixed) {
  tranposelist = list();
  rFileName = paste(rdirectory);
  print(independent)
  print(fixed)
  print(contour)
  inputnames = list("independent"=independent, "contour"=contour, "fixed"=fixed)

  for (variablename in names(inputnames))
  {
      for (name in names(functionnames))
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


drawGraph <-function(independent, fixed, contour, firstinputVector, secondinputVector, tableName, tabvalue, uniqueId)
{
  dir.create(imageDirectory);
  rfunctionname = getFunctionNameAsIs(independent, contour, fixed);
  drawfunctionname=getDrawFunctionName(drawfunctionprefix, tableName, rfunctionname);
  if (exists(as.character(substitute(drawfunctionname))))
  {
  	drawfunction <- get(drawfunctionname, mode="function");
  	imageFileName = paste(imageDirectory, tableName, uniqueId, "-", as.numeric(tabvalue), ".png", sep = '');
  	#png(file = imageFileName , units="in", width=10, height=8, res=150);
	png(file=imageFileName, width=500, height=500)
  	returnvalue<- drawfunction(firstinputVector,secondinputVector,as.numeric(tabvalue));
  #DrawDeltaSpecPPV(specmin, specmax, prev, ppv);
  	dev.off();
  	imageFileName;
  }
  else
  {
	print(paste("no Draw function available", drawfunctionname));
  }
}



getDrawFunctionName <- function (drawfunctionprefix, key, rfunctionname) {
  rDrawFileName = paste(rdirectory);
  rDrawFileName = paste(rDrawFileName, drawfunctionprefix, functionnames[[tolower(c(key))]], rfunctionname, sep = "")
  
  print(rDrawFileName)
  print(functionnames[[tolower(c(key))]])
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
