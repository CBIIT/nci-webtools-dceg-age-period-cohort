library('RJSONIO')
library('stringr')
source ('./r-code/SensSpecLR.R')

JsonWrapperLR <- function(sens,spec)
{
  data<- SensSpecLR(sens, spec);
  jsonString = "";
  jsonString=toJSON(round(data,3), method="C");
  str_replace_all(jsonString, "[\n]","");
}
