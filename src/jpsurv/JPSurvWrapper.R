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
