library('rjson')
library('JPSurv')
VERBOSE=FALSE

getDictionary <- function (inputFile, path, tokenId) {
  fqFileName = file.path(path, inputFile)
  outputFileName = paste("form-", tokenId, ".json", sep="")
  fqOutputFileName = file.path(path, outputFileName)
  seerFormData = dictionary.overview(fqFileName)
  cat(toJSON(seerFormData), file = fqOutputFileName)
  return(tokenId)
}

getSubsetStr <- function (yearOfDiagnosisVarName, yearOfDiagnosisRange, cohortVars, cohortValues) {

  yearOfDiagnosisVarName=paste0("`",getCorrectFormat(yearOfDiagnosisVarName), "`")
  startYearStr=paste(yearOfDiagnosisVarName, ">=", yearOfDiagnosisRange[1])
  endYearStr=paste(yearOfDiagnosisVarName, "<=", yearOfDiagnosisRange[2])
  yearStr=paste(startYearStr, endYearStr, sep='&')
  cohortVars=paste0("`",getCorrectFormat(cohortVars), "`")
  
  subsetStr=paste(paste(cohortVars, cohortValues, sep="=="), collapse='&')
  subsetStr=paste(subsetStr, yearStr, sep='&')

  #cat("*subsetStr\n")
  #cat(subsetStr)
  #cat("\n\n")
  return (subsetStr)

}

getFactorStr <- function (covariateVars) {
  factorStr=NULL
  if (length(covariateVars!=0L)) {
    covariateVars=paste0("`", getCorrectFormat(covariateVars), "`")
    factorStr=paste("~-1+", paste(gsub("$", ")", gsub("^", "factor(", covariateVars)), collapse="+"), sep='')
  }
  #cat("*factorStr\n")
  #cat(factorStr)
  #cat("\n\n")
  return (factorStr)
}

#replace empty space with _, strip anything other than alphanumeric _ /
getCorrectFormat <-function(variable) {
  variable=gsub("[^[:alnum:]_/]", "", gsub(" ", "_", variable))
  return (variable)
}

getGraphWrapper <- function (filePath, jpsurvDataString) {

  #print("R: getGraph")
  jpsurvData = fromJSON(jpsurvDataString)

  #print(jpsurvData$tokenId)
  #print("*jpsurvData.plot =")
  #print(jpsurvData$plot)


  fittedResultFile=paste("output-", jpsurvData$tokenId,".rds", sep="")

  intervals=c(5,10)
  intervals = jpsurvData$plot$form$intervals
  covariateValues = c("Localized", "Distant")
  covariateValues = jpsurvData$plot$form$covariateVars

  outputGraphFile = paste("plot-", jpsurvData$tokenId, "-",jpsurvData$plot$static$imageId, ".png", sep="")
  outputGraphFile = paste(filePath, outputGraphFile, sep="/")

  getGraph(filePath, fittedResultFile, intervals, covariateValues, outputGraphFile)

}

#filePath="."
#fittedResultFile="Breast_RelativeSurvival.output"
#intervals=c(5,10)
#covariateValues=c("Localized", "Distant")
#outputGraphFile="./Breast_RelativeSurvival123.png"
getGraph <- function (filePath, fittedResultFile, intervals, covariateValues, outputGraphFile) {

  #filePath="/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv/tmp"


  #cat("*** R variables ***\n")
  #cat("*intervals:\n")
  #print(intervals, row.names=FALSE)
  #cat("*covariateValues\n")
  #print(covariateValues, row.names=FALSE)
  #cat("\n")
  #cat("outputGraphFile:\n")
  #cat(outputGraphFile)
  #cat("\n")

  outFile=paste(filePath, fittedResultFile, sep="/" )

  outputData=readRDS(outFile)
  fit.result=outputData$fit.result
  continousVector=rep(NA, length(covariateValues))
  png(file=outputGraphFile)
  plot(fit.result,Intervals=intervals,covar.continuous=continousVector,covar.cat=covariateValues)
  dev.off()

}

getFittedResultWrapper <- function (filePath, jpsurvDataString) {

  jpsurvData = fromJSON(jpsurvDataString)

  seerFilePrefix = jpsurvData$calculate$static$seerFilePrefix
  yearOfDiagnosisVarName = jpsurvData$calculate$static$yearOfDiagnosisVarName
  yearOfDiagnosisRange = jpsurvData$calculate$form$yearOfDiagnosisRange
  allVars=jpsurvData$calculate$static$allVars
  cohortVars=jpsurvData$calculate$form$cohortVars
  cohortValues=jpsurvData$calculate$form$cohortValues
  covariateVars=jpsurvData$calculate$form$covariateVars
  numJP=jpsurvData$calculate$form$joinPoints
  fileName = paste('output', jpsurvData$tokenId, sep="-" )
  fileName = paste(fileName, "rds", sep="." )
  outputFileName = fileName

  fileName = paste('output', jpsurvData$tokenId, sep="-" )
  fileName = paste(fileName, "rds", sep="." )

  outputFileName =paste(filePath, fileName, sep="/" )

  return (getFittedResult(filePath, seerFilePrefix, yearOfDiagnosisVarName, yearOfDiagnosisRange, allVars, cohortVars, cohortValues, covariateVars, numJP, outputFileName))
}

#filePath="C:/devel/R"
#seerFilePrefix="SEER9_Survival_6CancerSitesByStage_1975_2007"
#yearOfDiagnosisVarName="Year of diagnosis (75-07 individual)"
#yearOfDiagnosisRange=c(1975, 2007)
#allVars=c("Sites: CR LB B O P T","Sex Male or Female","SEER historic stage A (All/loc/reg/dist)", "Year of diagnosis (75-07 individual)")
#cohortVars=c("Sites: CR LB B O P T")
#cohortValues=c("\"Colon and Rectum\"")
#covariateVars=c("SEER historic stage A (All/loc/reg/dist)")
#numJP=1
#outputFileName="SEER9_Survival_6CancerSitesByStage_1975_2007.output"

#filePath="C:/devel/R"
#seerFilePrefix="Breast_RelativeSurvival"
#yearOfDiagnosisVarName="Year of diagnosis 1975"
#yearOfDiagnosisRange=c(1975, 2011)
#allVars=c("Age groups","Breast stage","Year of diagnosis 1975")
#cohortVars=c("Age groups")
#cohortValues=c("\"65+\"")
#covariateVars=c("Breast stage")
#numJP=1
#outputFileName="Breast_RelativeSurvival.output"

getFittedResult <- function (filePath, seerFilePrefix, yearOfDiagnosisVarName, yearOfDiagnosisRange, allVars, cohortVars, cohortValues, covariateVars, numJP, outputFileName) {
  if(VERBOSE) {
  cat("*filePath\n")
  cat(filePath)
  cat("\n")
  cat("*seerFilePrefix\n")
  cat(seerFilePrefix)
  cat("\n")
  cat("*yearOfDiagnosisVarName\n")
  cat(yearOfDiagnosisVarName)
  cat("\n")
  cat("*yearOfDiagnosisRange")
  cat("\n")
  print(yearOfDiagnosisRange, row.names=FALSE)
  cat("\n")
  cat("*allVars\n")
  print(allVars, row.names=FALSE)
  cat("*cohortVars\n")
  print(cohortVars, row.names=FALSE)
  cat("*cohortValues\n")
  print(cohortValues, row.names=FALSE)
  cat("*covariateVars\n")
  print(covariateVars, row.names=FALSE)
  cat("*numJP\n")
  print(numJP, row.names=FALSE)
  cat("*outputFileName\n")

  print(outputFileName, row.names=FALSE)
  cat("\n****\n")
  }

  file=paste(filePath, seerFilePrefix, sep="/" )

  varLabels=getCorrectFormat(allVars)
  seerdata = joinpoint.seerdata(seerfilename=file,
                                newvarnames=varLabels,
                                NoFit=T,
                                UseVarLabelsInData=varLabels)

  subsetStr=getSubsetStr(yearOfDiagnosisVarName, yearOfDiagnosisRange, cohortVars, cohortValues)
  #assign subsetStr in the global in order for eval(parse(text=)) to work
  assign("subsetStr", subsetStr, envir = .GlobalEnv)

  factorStr=getFactorStr(covariateVars)
  assign("factorStr", factorStr, envir= .GlobalEnv)
  fit.result=joinpoint(seerdata,
                       subset = eval(parse(text=subsetStr)),
                       year=getCorrectFormat(yearOfDiagnosisVarName),
                       observedrelsurv="Relative_Survival_Cum",
                       model.form = eval(parse(text=factorStr)),
                       maxnum.jp=numJP);

  #save seerdata and fit.result as RData
  #cat("***outputFileName")
  #cat(outputFileName)
  #cat("\n")
  
  outputData=list("seerdata"=seerdata, "fit.result"=fit.result)
  saveRDS(outputData, outputFileName)
  #cat("\n\nOutput file has been written: ")
  #cat(outputFileName)
  #cat("\n")

  apcJson=paste(toJSON(fit.result$apc))
  return (apcJson)
}

getDownloadOutputWrapper <- function (filePath, jpsurvDataString) {
  #print("R: getDownloadOutputWrapper")
  jpsurvData = fromJSON(jpsurvDataString)
  #print(jpsurvData)

  #seerFilePrefix = jpsurvData$calculate$static$seerFilePrefix
  yearOfDiagnosisVarName = jpsurvData$calculate$static$yearOfDiagnosisVarName
  yearOfDiagnosisRange = jpsurvData$calculate$form$yearOfDiagnosisRange
  #allVars=jpsurvData$calculate$static$allVars
  cohortVars=jpsurvData$calculate$form$cohortVars
  cohortValues=jpsurvData$calculate$form$cohortValues
  #covariateVars=jpsurvData$calculate$form$covariateVars
  #numJP=jpsurvData$calculate$form$joinPoints
  subsetStr=getSubsetStr(yearOfDiagnosisVarName, yearOfDiagnosisRange, cohortVars, cohortValues)
  assign("subsetStr", subsetStr, envir = .GlobalEnv)

  fileName = paste('link', jpsurvData$tokenId, sep="-" )
  fileName = paste(fileName, "rds", sep="." )
 
  #outputFileName = fileName

  fileName = paste('link', jpsurvData$tokenId, sep="-" )
  fileName = paste(fileName, "rds", sep="." )
  outputFileName =paste(filePath, fileName, sep="/" )

  fittedResultFile=paste("output-", jpsurvData$tokenId,".rds", sep="")
  downloadFile=paste("link-", jpsurvData$tokenId,".csv", sep="")

  getDownloadOutput(filePath, fittedResultFile, subsetStr, downloadFile)

  return (downloadFile)
}

getDownloadOutput <- function(filePath, fittedResultFile, subsetStr, downloadFile) {
  downloadFile=paste(filePath, downloadFile, sep="/" )
  outFile=paste(filePath, fittedResultFile, sep="/" )
  outputData=readRDS(outFile)
  downloadOutput = output.overview(outputData$seerdata, outputData$fit.result, subsetStr);
  write.csv(downloadOutput, downloadFile)
}
