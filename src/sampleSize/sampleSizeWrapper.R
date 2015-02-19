source ('./DrawCompRecVark.R')

imageDirectory="./tmp/" 

#example input values
#k=c(0,1)
#sens=c(0.8, 0.9, 0.95, 0.995)
#spec=0.8
#prev=0.001
#N=1

#saveSensContours <- function()
saveSensContours <- function(k, sens, spec, prev, N, uniqueId, tabvalue)
{
  #k=c(0,1)
  #sens=c(0.8, 0.9, 0.95, 0.995)
  #spec=0.8
  #prev=0.001
  #N=1
  #uniqueId=1
  #save the PPV graph
  prepareSaveGraph(imageDirectory, "PPVkSensSpec-", uniqueId, tabvalue)
  DrawCompRecVarkSensSpec(k, sens, spec, prev, N)
  dev.off()
  
  #save the cNPV graph
  prepareSaveGraph(imageDirectory, "cNPVkSensSpec-", uniqueId, tabvalue)
  DrawCompRecVarcNPVkSensSpec(k, sens, spec, prev, N)
  dev.off()
}

#example input values
#k=c(0,1)
#spec=c(0.8, 0.9, 0.95, 0.995)
#sens=0.1
#prev=0.001
#N=1

saveSpecContours <- function(k, sens, spec, prev, N, uniqueId, tabvalue)
{
  #save the PPV graph
  prepareSaveGraph(imageDirectory, "PPVkSpecSens-", uniqueId, tabvalue)
  DrawCompRecVarkSpecSens(k, spec, sens, prev, N)
  dev.off()
  
  #save the cNPV graph
  prepareSaveGraph(imageDirectory, "cNPVkSpecSens-", uniqueId, tabvalue)
  DrawCompRecVarcNPVkSpecSens(k, spec, sens, prev, N)
  dev.off();
}

#preparation needed to save a graph as a file
prepareSaveGraph <- function(imgDir, graphPrefix, uniqueId, tabvalue) {
  dir.create(imgDir)
  graph=paste(imgDir, graphPrefix, uniqueId, "-", as.numeric(tabvalue),".png", sep='')
  png(file=graph)
}
