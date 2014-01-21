library('xlsx');

excelDirectory <- "/home/brent/myproject/nci-analysis-tools-web-presence/src/apc/static/xlsx/";

#excelDirectory <- "static/xlsx/";

writeResultsToExcel <- function (apcdata, uniqueId, imageDirectory) {
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
  
  addDataFrame(x = round(apcdata$AgeDeviations,3), sheet = ageDeviationsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$PerDeviations,3), sheet = perDeviationsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$CohDeviations,3), sheet = cohDeviationsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$LongAge,3), sheet = longAgeSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$CrossAge,3), sheet = crossAgeSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$Long2CrossRR,3), sheet = long2CrossRRSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$FittedTemporalTrends,3), sheet = fittedTemporalTrendsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$PeriodRR,3), sheet = periodRRSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$CohortRR,3), sheet = cohortRRSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$LocalDrifts, 3), sheet = localDriftsSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$NetDrift, 3), sheet = netDriftSheet,row.names=FALSE);
  addDataFrame(x = round(apcdata$Coefficients, 3), sheet = coefficientsSheet);
  addDataFrame(x = round(apcdata$Waldtests, 3), sheet = waldtestsSheet);
  
  addPicture(paste(imageDirectory, "AgeDeviations", uniqueId, ".png", sep = ''), ageDeviationsSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "PerDeviations", uniqueId, ".png", sep = ''), perDeviationsSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "CohDeviations", uniqueId, ".png", sep = ''), cohDeviationsSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "LongAge",       uniqueId, ".png", sep = ''), longAgeSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "CrossAge",      uniqueId, ".png", sep = ''), crossAgeSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "Long2CrossRR",  uniqueId, ".png", sep = ''), long2CrossRRSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "FittedTemporalTrends", uniqueId, ".png", sep = ''), fittedTemporalTrendsSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "PeriodRR",      uniqueId, ".png", sep = ''), periodRRSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "CohortRR",      uniqueId, ".png", sep = ''), cohortRRSheet, scale = .65, startRow = 1, startColumn = 6);
  addPicture(paste(imageDirectory, "LocalDrifts",   uniqueId, ".png", sep = ''), localDriftsSheet, scale = .65, startRow = 1, startColumn = 6);
  
  fileName <- paste(excelDirectory, uniqueId, '.xlsx',sep='');
  
  saveWorkbook(outwb, fileName);
  fileName;
}