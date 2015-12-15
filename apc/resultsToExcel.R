library('xlsx');

writeResultsToExcel <- function (apcdata, title, excelDirectory) {
  outwb <- createWorkbook();
  
  ageDeviationsSheet  <- createSheet(outwb, sheetName = "AgeDeviations");
  perDeviationsSheet  <- createSheet(outwb, sheetName ="PerDeviations");
  cohDeviationsSheet  <- createSheet(outwb, sheetName ="CohDeviations");
  longAgeSheet        <- createSheet(outwb, sheetName ="LongAge");
  crossAgeSheet       <- createSheet(outwb, sheetName ="CrossAge");
  long2CrossRRSheet   <- createSheet(outwb, sheetName ="Long2CrossRR");
  fittedTemporalTrendsSheet <- createSheet(outwb, sheetName ="FittedTemporalTrends");
  periodRRSheet       <- createSheet(outwb, sheetName ="PeriodRR");
  cohortRRSheet       <- createSheet(outwb, sheetName ="CohortRR");
  localDriftsSheet    <- createSheet(outwb, sheetName ="LocalDrifts");
  netDriftSheet       <- createSheet(outwb, sheetName ="NetDrift");
  coefficientsSheet   <- createSheet(outwb, sheetName = "Coefficients");
  waldtestsSheet      <- createSheet(outwb, sheetName = "Waldtests");
  
  addDataFrame(x = round(apcdata$AgeDeviations$table,3), sheet = ageDeviationsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$PerDeviations$table,3), sheet = perDeviationsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$CohDeviations$table,3), sheet = cohDeviationsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$LongAge$table,3), sheet = longAgeSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$CrossAge$table,3), sheet = crossAgeSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$Long2CrossRR$table,3), sheet = long2CrossRRSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$FittedTemporalTrends$table,3), sheet = fittedTemporalTrendsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$PeriodRR$table,3), sheet = periodRRSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$CohortRR$table,3), sheet = cohortRRSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$LocalDrifts$table, 3), sheet = localDriftsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$NetDrift$table, 3), sheet = netDriftSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$Coefficients$table, 3), sheet = coefficientsSheet);
  addDataFrame(x = round(apcdata$Waldtests$table, 3), sheet = waldtestsSheet);
  
  addPicture(apcdata$AgeDeviations$pathToFile        , ageDeviationsSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$PerDeviations$pathToFile        , perDeviationsSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$CohDeviations$pathToFile        , cohDeviationsSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$LongAge$pathToFile              , longAgeSheet,       scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$CrossAge$pathToFile             , crossAgeSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$Long2CrossRR$pathToFile         , long2CrossRRSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$FittedTemporalTrends$pathToFile , fittedTemporalTrendsSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$PeriodRR$pathToFile             , periodRRSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$CohortRR$pathToFile             , cohortRRSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(apcdata$LocalDrifts$pathToFile          , localDriftsSheet, scale = .65, startRow = 1, startColumn = 6);
  
  time <- gsub(":","",gsub("-","",gsub(" ","", Sys.time() , fixed=TRUE)));
  title <- gsub(" ", "", gsub("[[:punct:]]", "", title));
  
  defaultTitle<-pmatch("APCAnalysis", title, nomatch=0);
  
  if (defaultTitle > 0) {
    fileName <- paste(excelDirectory, title,"_", 'Excel.xlsx',sep='');
  } else {
    fileName <- paste(excelDirectory, title,"_",time,"_", 'Excel.xlsx',sep='');
  }
  
  saveWorkbook(outwb, fileName);
  fileName;
}