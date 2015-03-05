library('rjson')
library('JPSurv')

imageDirectory="./tmp/"

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

getFittedResult <- function (filePath, seerFilePrefix, yearOfDiagnosisVarName, yearOfDiagnosisRange, allVars, cohortVars, cohortValues, covariateVars, numJP, outputFileName) {

  #filePath="C:/devel/R"
  #seerFilePrefix="Breast_RelativeSurvival"
  yearOfDiagnosisVarName="Year_of_diagnosis_1975"
  yearOfDiagnosisRange=c(1975, 2011)
  allVars=c("Age_groups","Breast_stage","Year_of_diagnosis_1975")
  cohortVars=c("Age_groups")
  cohortValues=c("\"65+\"")
  covariateVars=c("Breast_stage")
  numJP=1
  outputFileName="Breast_RelativeSurvival.output"
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
  varLabels=gsub(" ", "_", gsub(" $", "", gsub("[^[:alnum:]]", " ", allVars)))
  cat("*varLabels\n")
  cat(varLabels)
  cat("\n\n")

  seerdata = joinpoint.seerdata(seerfilename=file,
                                newvarnames=allVars,
                                NoFit=T,
                                UseVarLabelsInData=varLabels,
                                yearOfDiagnosisVarName)

  subsetStr=getSubsetStr(yearOfDiagnosisVarName, yearOfDiagnosisRange, cohortVars, cohortValues)
  #assign subsetStr in the global in order for eval(parse(text=)) to work
  assign("subsetStr", subsetStr, envir = .GlobalEnv)

  #fit.result = joinpoint(seerdata, subset = subsetStr,
  #                       year=yearOfDiagnosisVarName,
  #                       observedrelsurv="Relative_Survival_Cum",
  #                       model.form = ~-1+factor(factorStr),
  #                       maxnum.jp = numJP);

  factorStr=getFactorStr(covariateVars)
  assign("factorStr", factorStr, envir= .GlobalEnv)
  fit.result=joinpoint(seerdata,
                       subset = eval(parse(text=subsetStr)),
                       year=yearOfDiagnosisVarName,
                       observedrelsurv="Relative_Survival_Cum",
                       model.form = eval(parse(text=factorStr)),
                       maxnum.jp=numJP);

  #save fit.result as RData
  saveRDS(fit.result, outputFileName)

  apcJson=cat(toJSON(fit.result$apc), .escapeEscapes=TRUE)
  return (apcJson)
}

getSubsetStr <- function (yearOfDiagnosisVarName, yearOfDiagnosisRange, cohortVars, cohortValues) {
  startYearStr=paste(yearOfDiagnosisVarName, ">=", yearOfDiagnosisRange[1])
  endYearStr=paste(yearOfDiagnosisVarName, "<=", yearOfDiagnosisRange[2])
  yearStr=paste(startYearStr, endYearStr, sep='&')
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
    factorStr=paste("~-1+", paste(gsub("$", ")", gsub("^", "factor(", covariateVars)), collapse="+"), sep='')
  }
  cat("*factorStr\n")
  cat(factorStr)
  cat("\n\n")
  return (factorStr)
}
