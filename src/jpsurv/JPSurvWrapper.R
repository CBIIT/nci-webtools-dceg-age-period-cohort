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

  randomNumber = paste( sample( 0:9, 6, replace=TRUE ), collapse="" )
  cat("\nrandomNumber\n")
  cat(randomNumber)
  cat("\n")

  outputFileName = paste("output-", randomNumber, ".json", sep="")
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
  #Return the outputFileName
  return(outputFileName)
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


#seerdata = joinpoint.seerdata(seerfilename="C:/devel/R/Breast_RelativeSurvival", 
#                             newvarnames=c("Age groups","Breast stage","Year of diagnosis 1975+"),NoFit=T,
#                             UseVarLabelsInData=c("Age_groups","Breast_stage", "Year_of_diagnosis_1975")
#                             );

# fit the joinpoint model with joinpoin.surv
#fit.result = joinpoint(seerdata, subset = Age_groups == "65+" & Year_of_diagnosis_1975 >=2001,
#                       year="Year_of_diagnosis_1975",observedrelsurv="Relative_Survival_Cum",
#                       model.form = ~-1+factor(Breast_stage),
#                       maxnum.jp = 1);

getFittedResult <- function (filePath, seerFilePrefix, yearOfDiagnosisVarName, yearOfDiagnosisRange, allVars, cohortVars, cohortValues, covariateVars, numJP, outputFileName) {
 
  #filePath="C:/devel/R"
  #seerFilePrefix="Breast_RelativeSurvival"
  #yearOfDiagnosisVarName="Year_of_diagnosis_1975"
  #yearofDiagnosisRange=c(1975, 2011)
  #allVar=c("Age_groups","Breast_stage","Year_of_diagnosis_1975")
  #cohortVars=c("Age_groups")
  #cohortValues=c("\"65+\"")
  #covariateVars=c("Breast_stage") 
  #numJP=1
  #outputFileName="Breast_RelativeSurvival.output"
  
  file=paste(filePath, seerFilePrefix, sep="/" )
  
  varLabels=gsub(" ", "_", gsub(" $", "", gsub("[^[:alnum:]]", " ", varNames)))
  
  seerdata = joinpoint.seerdata(seerfilename=file, 
                                newvarnames=varNames,
                                NoFit=T,
                                UseVarLabelsInData=varLabels,
                                yearOfDiagnosisVarName)
  
  #fit.result = joinpoint(seerdata, subset = subsetStr,
  #                       year=yearOfDiagnosisVarName,
  #                       observedrelsurv="Relative_Survival_Cum",
  #                       model.form = ~-1+factor(factorStr),
  #                       maxnum.jp = numJP);
  
  startYearStr=paste(yearOfDiagnosisVarName, ">=", yearofDiagnosisRange[1])
  endYearStr=paste(yearOfDiagnosisVarName, "<=", yearofDiagnosisRange[2])
  yearStr=paste(startYearStr, endYearStr, sep='&')
  subsetStr=paste(paste(cohortVars, cohortValues, sep="=="), collapse='&')
  subsetStr=paste(subsetStr, yearStr, sep='&')
  
  fit.result=joinpoint(seerdata,
                       subset = eval(parse(text=subsetStr)),
                       year=yearOfDiagnosisVarName,
                       observedrelsurv="Relative_Survival_Cum",
                       model.form = ~-1+factor(Breast_stage),
                       maxnum.jp=numJP);
  
  #save fit.result as RData
  saveRDS(fit.result, outputFileName)
  
  apcJson=cat(toJSON(fit.result$apc), .escapeEscapes=TRUE)
  return (apcJson)
}


filePath="C:/devel/R"
seerFilePrefix="Breast_RelativeSurvival"
yearOfDiagnosisVarName="Year_of_diagnosis_1975"
yearofDiagnosisRange=c(1975, 2011)
allVar=c("Age_groups","Breast_stage","Year_of_diagnosis_1975")
cohortVars=c("Age_groups")
cohortValues=c("\"65+\"")
covariateVars=c("Breast_stage") 
numJP=1
outputFileName="Breast_RelativeSurvival.output"