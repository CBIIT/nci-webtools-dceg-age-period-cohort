library('rjson')
library('JPSurv')

getDictionary <- function (inputFile, path, tokenId) {
  #e.g.
  cat("\nFrom R:  getDictionary\n")
  cat("inputFile\n")
  cat(inputFile)
  cat("\n")
  cat("path\n")
  cat(path)
  cat("\n")
  fqFileName = file.path(path, inputFile)
  cat("fqFileName\n")
  cat(fqFileName)
  cat("\n")

  #keyId = paste( sample( 0:9, 6, replace=TRUE ), collapse="" )
  cat("\ntokenId\n")
  cat(tokenId)
  cat("\n")

  outputFileName = paste("form-", tokenId, ".json", sep="")
  cat("\noutputFileName\n")
  cat(outputFileName)
  cat("\n")
  fqOutputFileName = file.path(path, outputFileName)
  cat("\nfqOutputFileName\n")
  cat(fqOutputFileName)
  cat("\n")

  seerFormData = dictionary.overview(fqFileName)
  #
  #Write json data to outputfile
  #
  cat(toJSON(seerFormData), file = fqOutputFileName)
  #Return the keyId
  return(tokenId)
}

#filePath="C:/devel/R"
#seerFilePrefix="SEER9_Survival_6CancerSitesByStage_1975_2007"
#yearOfDiagnosisVarName="Year of diagnosis (75-07 individual)"
#yearOfDiagnosisRange=c(1975, 2011)
#allVars=c("Sites: CR LB B O P T","Sex Male or Female","SEER historic stage A (All/loc/reg/dist)", "Year of diagnosis (75-07 individual)")
#cohortVars=c("Sites: CR LB B O P T")
#cohortValues=c("\"Colon and Rectum\"")
#covariateVars=c("Sex Male or Female")
#numJP=1
#outputFileName="SEER9_Survival_6CancerSitesByStage_1975_2007.output"

#filePath="C:/devel/R"
#jpsurvDataString = "Hello"
#seerFilePrefix="Breast_RelativeSurvival"
#yearOfDiagnosisVarName="Year of diagnosis 1975"
#yearOfDiagnosisRange=c(1975, 2011)
#allVars=c("Age groups","Breast stage","Year of diagnosis 1975")
#cohortVars=c("Age groups")
#cohortValues=c("\"65+\"")
#covariateVars=c("Breast stage")
#numJP=1
#outputFileName="Breast_RelativeSurvival.output"

#getFittedResult <- function (filePath, seerFilePrefix, yearOfDiagnosisVarName, yearOfDiagnosisRange, allVars, cohortVars, cohortValues, covariateVars, numJP, outputFileName) {
getFittedResultJSON <- function (filePath, jpsurvDataString) {
#getFittedResults <- function () {
#filePath="/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv/tmp"
print("R: getFittedResult")
jpsurvData = fromJSON(jpsurvDataString)
print(jpsurvData)
#print(jpsurvData$tokenId)
#print(jpsurvData$status)

#seerFilePrefix="Breast_RelativeSurvival"
seerFilePrefix = jpsurvData$calculate$static$seerFilePrefix
yearOfDiagnosisVarName="Year of diagnosis 1975"
yearOfDiagnosisVarName = jpsurvData$calculate$static$yearOfDiagnosisVarName

yearOfDiagnosisRange=c(1975, 2011)
yearOfDiagnosisRange = jpsurvData$calculate$form$yearOfDiagnosisRange
allVars=c("Age groups","Breast stage","Year of diagnosis 1975")
allVars=jpsurvData$calculate$static$allVars
cohortVars=c("Age groups")
cohortVars=jpsurvData$calculate$form$cohortVars
cohortValues=c("\"65+\"")
cohortValues=c("\"Localized\"")
cohortValues=jpsurvData$calculate$form$cohortValues
covariateVars=c("Breast stage")
covariateVars=jpsurvData$calculate$form$covariateVars
joinPoints=1
joinPoints=jpsurvData$calculate$form$joinPoints
fileName = paste('output', jpsurvData$tokenId, sep="-" )
fileName = paste(fileName, "rds", sep="." )
#fileName = paste("joinpoint-output", "txt", sep="." )
outputFileName =paste(filePath, fileName, sep="/" )

  cat("*jpsurvDataString\n")
  cat(jpsurvDataString)
  cat("\n")
  cat("*jpsurvData\n")
  print(jpsurvData)
  cat("\n")
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
  cat("*joinPoints\n")
  print(joinPoints, row.names=FALSE)
  cat("*outputFileName\n")

  print(outputFileName, row.names=FALSE)
  cat("\n****\n")

  file=paste(filePath, seerFilePrefix, sep="/" )
  cat("*file\n")
  cat(file)
  cat("\n\n")

  varLabels=getCorrectFormat(allVars)

  cat("*varLabels\n")
  cat(allVars)
  cat("\n\n")

  seerdata = joinpoint.seerdata(seerfilename=file,
                                newvarnames=varLabels,
                                NoFit=T,
                                UseVarLabelsInData=varLabels)

  subsetStr=getSubsetStr(yearOfDiagnosisVarName, yearOfDiagnosisRange, cohortVars, cohortValues)
  #assign subsetStr in the global in order for eval(parse(text=)) to work
  assign("subsetStr", subsetStr, envir = .GlobalEnv)
  cat("***subsetStr\n")
  cat(subsetStr)
  cat("\n")

  factorStr=getFactorStr(covariateVars)
  assign("factorStr", factorStr, envir= .GlobalEnv)
  cat("***factorStr\n")
  cat(factorStr)
  cat("\n")

  fit.result=joinpoint(seerdata,
                       subset = eval(parse(text=subsetStr)),
                       year=getCorrectFormat(yearOfDiagnosisVarName),
                       observedrelsurv="Relative_Survival_Cum",
                       model.form = eval(parse(text=factorStr)),
                       maxnum.jp=joinPoints);

  #save fit.result as RData
  saveRDS(fit.result, outputFileName)
  cat("\n\nOutput file has been written: ")
  cat(outputFileName)
  cat("\n")

  #apcJson=cat(toJSON(fit.result$apc), .escapeEscapes=TRUE)
  apcJson=paste(toJSON(fit.result$apc))
  cat("\nHERE IS THE JSON output for stage2: calculate\n")
  print(apcJson)
  cat("\n")
  return (apcJson)
}

getSubsetStr <- function (yearOfDiagnosisVarName, yearOfDiagnosisRange, cohortVars, cohortValues) {

  yearOfDiagnosisVarName=getCorrectFormat(yearOfDiagnosisVarName)
  startYearStr=paste(yearOfDiagnosisVarName, ">=", yearOfDiagnosisRange[1])
  endYearStr=paste(yearOfDiagnosisVarName, "<=", yearOfDiagnosisRange[2])
  yearStr=paste(startYearStr, endYearStr, sep='&')
  cohortVars=getCorrectFormat(cohortVars)

  subsetStr=paste(paste(cohortVars, cohortValues, sep="=="), collapse='&')
  subsetStr=paste(subsetStr, yearStr, sep='&')

  cat("*subsetStr\n")
  cat(subsetStr)
  cat("\n\n")
  return (subsetStr)
}

getFactorStr <- function (covariateVars) {
  factorStr=NULL
  if (length(covariateVars!=0L)) {
    covariateVars=getCorrectFormat(covariateVars)
    factorStr=paste("~-1+", paste(gsub("$", ")", gsub("^", "factor(", covariateVars)), collapse="+"), sep='')
  }
  cat("*factorStr\n")
  cat(factorStr)
  cat("\n\n")
  return (factorStr)
}

#replace empty space with _, strip anything other than alphanumeric _ /
getCorrectFormat <-function(variable) {
  variable=gsub("[^[:alnum:]_/]", "", gsub(" ", "_", variable))
  return (variable)
}

#filePath="."
#fittedResultFile="Breast_RelativeSurvival.output"
#intervals=c(5,10)
#covariateValues=c("Localized", "Distant")
#outputGraphFile="./Breast_RelativeSurvival123.png"

getGraph <- function (filePath, jpsurvDataString) {

  #filePath="/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv/tmp"
  print("R: getGraph")
  jpsurvData = fromJSON(jpsurvDataString)

  print(jpsurvData$tokenId)
  print("*jpsurvData.plot =")
  print(jpsurvData$plot)

  fittedResultFile=paste("output-", jpsurvData$tokenId,".rds", sep="")

  intervals=c(5,10)
  intervals = jpsurvData$plot$form$intervals
  covariateValues = c("Localized", "Distant")
  covariateValues = jpsurvData$plot$form$covariateVars

  outputGraphFile = paste("plot-", jpsurvData$tokenId, "-",jpsurvData$plot$static$imageId, ".png", sep="")
  outputGraphFile = paste(filePath, outputGraphFile, sep="/")

  cat("*** R variables ***")
  cat("*intervals:\n")
  print(intervals, row.names=FALSE)
  cat("*covariateValues\n")
  print(covariateValues, row.names=FALSE)
  cat("\n")
  cat("outputGraphFile:\n")
  cat(outputGraphFile)
  cat("\n")

  outFile=paste(filePath, fittedResultFile, sep="/" )

  fit.result=readRDS(outFile)
  continousVector=rep(NA, length(covariateValues))
  png(file=outputGraphFile)
  plot(fit.result,Intervals=intervals,covar.continuous=continousVector,covar.cat=covariateValues);
  dev.off()

}

#filePath="C:/devel/R"
#seerFilePrefix="SEER9_Survival_6CancerSitesByStage_1975_2007"
#yearOfDiagnosisVarName="Year of diagnosis (75-07 individual)"
#yearOfDiagnosisRange=c(1975, 2011)
#allVars=c("Sites: CR LB B O P T","Sex Male or Female","SEER historic stage A (All/loc/reg/dist)", "Year of diagnosis (75-07 individual)")
#cohortVars=c("Sites: CR LB B O P T")
#cohortValues=c("\"Colon and Rectum\"")
#covariateVars=c("Sex Male or Female")
#numJP=1
#outputFileName="SEER9_Survival_6CancerSitesByStage_1975_2007.output"

#filePath="C:/devel/R"
#jpsurvDataString = "Hello"
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
#filePath="/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv/tmp"

#seerFilePrefix="Breast_RelativeSurvival"
#yearOfDiagnosisVarName="Year of diagnosis 1975"
#yearOfDiagnosisRange=c(1975, 2011)
#allVars=c("Age groups","Breast stage","Year of diagnosis 1975")
#cohortVars=c("Age groups")
#cohortValues=c("\"65+\"")
#cohortValues=c("\"Localized\"")
#covariateVars=c("Breast stage")
#joinPoints=1
#fileName = paste('output', seerFilePrefix, sep="-" )
#fileName = paste(fileName, "rds", sep="." )

  outputFileName =paste(filePath, fileName, sep="/" )
  file=paste(filePath, seerFilePrefix, sep="/" )
  varLabels=getCorrectFormat(allVars)
  seerdata = joinpoint.seerdata(seerfilename=file,
                                newvarnames=varLabels,
                                NoFit=T,
                                UseVarLabelsInData=varLabels)

  subsetStr=getSubsetStr(yearOfDiagnosisVarName, yearOfDiagnosisRange, cohortVars, cohortValues)
  #assign subsetStr in the global in order for eval(parse(text=)) to work
  factorStr=getFactorStr(covariateVars)
  fit.result=joinpoint(seerdata,
                       subset = eval(parse(text=subsetStr)),
                       year=getCorrectFormat(yearOfDiagnosisVarName),
                       observedrelsurv="Relative_Survival_Cum",
                       model.form = eval(parse(text=factorStr)),
                       maxnum.jp=joinPoints);

  #save fit.result as RData
  saveRDS(fit.result, outputFileName)

  apcJson=paste(toJSON(fit.result$apc))
  return (apcJson)
}
