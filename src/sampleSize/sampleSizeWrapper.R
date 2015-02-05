source ('./DrawCompRecVark.R')

imageDirectory="./tmp/" 

#example input values
#k=c(0,1)
#sens=c(0.8, 0.9, 0.95, 0.995)
#spec=0.8
#prev=0.001
#N=1

saveSensContours <- function(k, sens, spec, prev, N, uniqueId)
{
  #save the PPV graph
  prepareSaveGraph(imageDirectory, "PPVkSensSpec-", uniqueId)
  DrawCompRecVarkSensSpec(k, sens, spec, prev, N)
  dev.off()
  
  #save the cNPV graph
  prepareSaveGraph(imageDirectory, "cNPVkSensSpec-", uniqueId)
  DrawCompRecVarcNPVkSensSpec(k, sens, spec, prev, N)
  dev.off()
}

#example input values
#k=c(0,1)
#spec=c(0.8, 0.9, 0.95, 0.995)
#sens=0.1
#prev=0.001
#N=1

saveSpecContours <- function(k, sens, spec, prev, N, uniqueId)
{
  #save the PPV graph
  prepareSaveGraph(imageDirectory, "PPVkSpecSens-", uniqueId)
  DrawCompRecVarkSensSpec(k, sens, spec, prev, N)
  dev.off()
  
  #save the cNPV graph
  prepareSaveGraph(imageDirectory, "cNPVkSpecSens-", uniqueId)
  DrawCompRecVarcNPVkSensSpec(k, sens, spec, prev, N)
  dev.off();
}

#preparation needed to save a graph as a file
prepareSaveGraph <- function(imgDir, graphPrefix, uniqueId) {
  dir.create(imgDir)
  graph=paste(imgDir, graphPrefix, uniqueId, ".png", sep='')
  png(file=graph)
}
