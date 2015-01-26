library('RJSONIO');
library(JPSurv);

# Make sure the working directory is set to where the dictionary file is saved.

dictionary = dictionary.overview("C:/devel/R/Breast_RelativeSurvival.dic");

# DICsample is a list of the various sections of the .dic file. The names of the sections can be found by expanding 
# the object in the Environment tab in R Studio.
# For your use I think you will be interested in: VarAllInfo, VarFormatSecList, VarLabelInfo.

#variable info 
#transpose so that the var names are the column headers
#varInfo=setNames(data.frame(t(dictionary$VarAllInfo[, -1])), dictionary$VarAllInfo[,1])

dictionaryJSON=cat(toJSON(dictionary), .escapeEscapes=TRUE)