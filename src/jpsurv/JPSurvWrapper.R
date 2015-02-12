library('RJSONIO');
library('JPSurv');


getDictionaryAsJson <- function (fullPathDictionaryFile) {
  #e.g.
  #dictionary = dictionary.overview("C:/devel/R/Breast_RelativeSurvival.dic");
  dictionary = dictionary.overview(fullPathDictionaryFile)

  #translate to JSON as is
  dictionaryJSON=cat(toJSON(dictionary), .escapeEscapes=TRUE)
  
  #transpose so that the var names are the column headers
  #varInfo=setNames(data.frame(t(dictionary$VarAllInfo[, -1])), dictionary$VarAllInfo[,1])
  #this is not being used right now
  
  return(dictionaryJSON)
}



