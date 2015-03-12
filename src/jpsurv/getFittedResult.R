library('rjson')
library('JPSurv')

#replace empty space with _, strip anything other than alphanumeric _ /
getCorrectFormat <-function(variable) {
  variable=gsub("[^[:alnum:]_/]", "", gsub(" ", "_", variable))
  return (variable)
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


filePath="/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv/tmp"
seerFilePrefix="Breast_RelativeSurvival"
yearOfDiagnosisVarName="Year of diagnosis 1975"
yearOfDiagnosisRange=c(1975, 2011)
allVars=c("Age groups","Breast stage","Year of diagnosis 1975")
cohortVars=c("Age groups")
cohortValues=c("\"65+\"")
covariateVars=c("Breast stage")
numJP=1
outputFileName="Breast_RelativeSurvival.output"

#getFittedResult <- function (filePath, seerFilePrefix, yearOfDiagnosisVarName, yearOfDiagnosisRange, allVars, cohortVars, cohortValues, covariateVars, numJP, outputFileName) {

  print("R: getFittedResult")
  print("Here are the current paths to library")
  .libPaths()

#seerFilePrefix="Breast_RelativeSurvival"
#yearOfDiagnosisVarName="Year of diagnosis 1975"
#yearOfDiagnosisRange=c(1975, 2011)
allVars=c("Age groups","Breast stage","Year of diagnosis 1975")
cohortVars=c("Age groups")
cohortValues=c("\"65+\"")
covariateVars=c("Breast stage")
numJP=1
#outputFileName="Breast_RelativeSurvival.output"

  cat("*filePath\n")
  cat(filePath)
  cat("\n")
  cat("*seerFilePrefix\n")
  cat(seerFilePrefix)
  cat("\n")
  cat("*yearOfDiagnosisVarName\n")
  cat(yearOfDiagnosisVarName)
  cat("\n")
  print("*yearOfDiagnosisRange")
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

  factorStr=getFactorStr(covariateVars)
  assign("factorStr", factorStr, envir= .GlobalEnv)

  fit.result=joinpoint(seerdata,
                       subset = eval(parse(text=subsetStr)),
                       year=getCorrectFormat(yearOfDiagnosisVarName),
                       observedrelsurv="Relative_Survival_Cum",
                       model.form = eval(parse(text=factorStr)),
                       maxnum.jp=numJP);

  #save fit.result as RData
  cat("Saving printed results in the outputFileName\n")
  cat(outputFileName)
  cat("\n")
  cat("fit.Result\n")
  #cat(fit.result)
  cat("\n")

  saveRDS(fit.result, outputFileName)
  outputFileNameJson = paste(outputFileName, "json", sep=".")

  apcJson<-toJSON(fit.result$apc, .escapeEscapes = FALSE, pretty=TRUE)

  cat("apc\n")
  cat(apcJson)
  cat("\n")
  #cat(apcJson, file=outputFileNameJson, append=TRUE)
  fileConn<-file(outputFileNameJson)
  writeLines(apcJson, fileConn)
  close(fileConn)
