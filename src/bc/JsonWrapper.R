library('RJSONIO')
library('stringr')
source ('./SensSpecLRPV.R')

JsonWrapper <- function(sens,spec,prev)
{
  data<- SensSpecLRPV(sens, spec, prev);
  jsonString = "";
  jsonString=toJSON(round(data,3), method="C");
  str_replace_all(jsonString, "[\n]","");
}
