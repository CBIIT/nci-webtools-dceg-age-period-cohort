library('rjson')
library('JPSurv')

imageDirectory="./tmp/"

getDictionaryAsJson2 <- function () {
  #e.g.
  cat("From R:  getDictionaryAsJson2\n")
  cat("dictionary:\n")
  dictionary = dictionary.overview("/h1/kneislercp/nci-analysis-tools-web-presence/src/jpsurv/tmp/example1.txt")
  cat(toJSON(dictionary), .escapeEscapes=FALSE)
  cat("Print dictionary:\n")
  cat("\n\n")
  #translate to JSON as is
  dictionaryJSON=cat(toJSON(dictionary), .escapeEscapes=TRUE)

  #transpose so that the var names are the column headers
  #varInfo=setNames(data.frame(t(dictionary$VarAllInfo[, -1])), dictionary$VarAllInfo[,1])
  #this is not being used right now

  return(dictionary)
}

getDictionary <- function (inputFile, path) {
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

  keyId = paste( sample( 0:9, 6, replace=TRUE ), collapse="" )
  cat("\nkeyId\n")
  cat(keyId)
  cat("\n")

  outputFileName = paste("output-", keyId, ".json", sep="")
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
  return(keyId)
}

if(FALSE) {
getDictionaryAsJson <- function (fullPathDictionaryFile) {
  #e.g.
  #dictionary = dictionary.overview("C:/devel/R/Breast_RelativeSurvival.dic");
  cat("From R:  getDictionaryAsJson\n")
  cat(fullPathDictionaryFile)
  dictionary = dictionary.overview(fullPathDictionaryFile)

  #translate to JSON as is
  dictionaryJSON=cat(toJSON(dictionary), .escapeEscapes=TRUE)

  #transpose so that the var names are the column headers
  #varInfo=setNames(data.frame(t(dictionary$VarAllInfo[, -1])), dictionary$VarAllInfo[,1])
  #this is not being used right now

  return(dictionaryJSON)
}
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
  saveRDS(fit.result, outputFileName)

  apcJson=cat(toJSON(fit.result$apc), .escapeEscapes=TRUE)
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

getGraph <- function (filePath, fittedResultFile, intervals, covariateValues, outputGraphFile) {
  outFile=paste(filePath, fittedResultFile, sep="/" )
  fit.result=readRDS(outFile)
  continousVector=rep(NA, length(covariateValues))  
  png(file=outputGraphFile)
  plot(fit.result,Intervals=intervals,covar.continuous=continousVector,covar.cat=covariateValues);
  dev.off()
}