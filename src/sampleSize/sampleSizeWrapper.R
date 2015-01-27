source ('./DrawCompReVark.R')

#example input values
k=c(0,1)
sens=c(0.8, 0.9, 0.95, 0.995)
spec=0.8
prev=0.001
N=1

imageDirectory="./tmp/" 
uniqueId=1
dir.create(imageDirectory)

#save the PPV graph
ppvGraph = paste(imageDirectory, "PPVkSensSpec-", uniqueId, ".png", sep = '')
png(file=ppvGraph)
DrawCompRecVarkSensSpec(k, sens, spec, prev, N)
dev.off();

#save the cNPV graph
cnpvGraph = paste(imageDirectory, "cNPVkSensSpec-", uniqueId, ".png", sep = '')
png(file=cnpvGraph)
DrawCompRecVarcNPVkSensSpec(k, sens, spec, prev, N)
dev.off();

#example input values
k=c(0,1)
spec=c(0.8, 0.9, 0.95, 0.995)
sens=0.1
prev=0.001
N=1

uniqueId=2

#save the PPV graph
ppvGraph = paste(imageDirectory, "PPVkSpecSens-", uniqueId, ".png", sep = '')
png(file=ppvGraph)
DrawCompRecVarkSpecSens(k, spec, sens, prev, N)
dev.off();

#save the cNPV graph
cnpvGraph = paste(imageDirectory, "cNPVkSpecSens-", uniqueId, ".png", sep = '')
png(file=cnpvGraph)
DrawCompRecVarcNPVkSpecSens(k, spec, sens, prev, N)
dev.off();
