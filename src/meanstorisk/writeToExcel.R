library('xlsx');

#excelDirectory <- "/home/brent/development/nci-analysis-tools-web-presence/src/meanstorisk/xlsx/";

excelDirectory <- "./xlsx/";

writeResultsToExcel <- function (risk, graphName) {
  outwb <- createWorkbook();
  
  deltaSheet            <- createSheet(outwb, sheetName = "Delta");

  ppvSheet              <- createSheet(outwb, sheetName = "PPV");
  cNpvSheet             <- createSheet(outwb, sheetName = "cNPV");
  ppvCnpvSheet          <- createSheet(outwb, sheetName = "PPV-cNPV");
  programBasedSheet     <- createSheet(outwb, sheetName = "Program Based");
  ppvBasedSheet         <- createSheet(outwb, sheetName = "PPV Based");
  sensitivityBasedSheet <- createSheet(outwb, sheetName = "Sensitivity Based");
  dominatedSheet        <- createSheet(outwb, sheetName = "Dominated by Specificity for a Rare Disease");
  
  addDataFrame(x = as.data.frame.matrix(risk$Delta), sheet = deltaSheet, row.names=TRUE, col.name=TRUE);
  
  addDataFrame(x = as.data.frame.matrix(risk$`Sensitivity Given Specificity`), sheet = ppvSheet, row.names=TRUE, col.name=TRUE);
  addDataFrame(x = as.data.frame.matrix(risk$PPV), sheet = ppvSheet, row.names=FALSE, col.name=TRUE,startColumn=6);
  
  addDataFrame(x = as.data.frame.matrix(risk$`Sensitivity Given Specificity`), sheet = cNpvSheet, row.names=TRUE, col.name=TRUE);
  addDataFrame(x = as.data.frame.matrix(risk$cNPV), sheet = cNpvSheet, row.names=FALSE, col.name=TRUE, startColumn=6);
  
  addDataFrame(x = as.data.frame.matrix(risk$`Sensitivity Given Specificity`), sheet = ppvCnpvSheet, row.names=TRUE, col.name=TRUE);
  addDataFrame(x = as.data.frame.matrix(risk$`PPV-cNPV`), sheet = ppvCnpvSheet, row.names=FALSE, col.name=TRUE, startColumn=6);
  
  addDataFrame(x = as.data.frame.matrix(risk$`Sensitivity Given Specificity`), sheet = programBasedSheet, row.names=TRUE, col.name=TRUE);
  addDataFrame(x = as.data.frame.matrix(risk$`Program-Based`), sheet = programBasedSheet, row.names=FALSE, col.name=TRUE, startColumn=6);
  
  addDataFrame(x = as.data.frame.matrix(risk$`Sensitivity Given Specificity`), sheet = ppvBasedSheet, row.names=TRUE, col.name=TRUE);
  addDataFrame(x = as.data.frame.matrix(risk$`PPV-Based`), sheet = ppvBasedSheet, row.names=FALSE, col.name=TRUE, startColumn=6);
  
  addDataFrame(x = as.data.frame.matrix(risk$`Sensitivity Given Specificity`), sheet = sensitivityBasedSheet, row.names=TRUE, col.name=TRUE);
  addDataFrame(x = as.data.frame.matrix(risk$`Sensitivity-Based`), sheet = sensitivityBasedSheet, row.names=FALSE, col.name=TRUE, startColumn=6);
  
  addDataFrame(x = as.data.frame.matrix(risk$`Sensitivity Given Specificity`), sheet = dominatedSheet, row.names=TRUE, col.name=TRUE);
  addDataFrame(x = as.data.frame.matrix(risk$`Dominated by Specificity for a Rare Disease`), sheet = dominatedSheet, row.names=FALSE, col.name=TRUE, startColumn=6);
  
  addPicture(graphName, deltaSheet, scale = .65, startRow = 1, startColumn = 6);

   time <- gsub(":","",gsub("-","",gsub(" ","", Sys.time() , fixed=TRUE)));

  fileName <- paste(excelDirectory, time,"_", 'Excel.xlsx',sep='');
  
  saveWorkbook(outwb, fileName);
  fileName;
}