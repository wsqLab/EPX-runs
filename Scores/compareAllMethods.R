#code for 10-fold CV where entirely new EPX model is built in each CV fold (ie for 3 replications of 10-fold CV, 30 EPX models are built)

#load libraries and functions
library(EPX)
library(randomForest)
library(doParallel)
source("~/RCode/git/compareAllMethodsFunctions.R")

#read and prepare data
dat = read.csv("~/data/AID348/PH348.csv")
dat = dat[, 2:ncol(dat)]
dat$y = y
initialGroups = getGroups(dat) #c(1:(ncol(dat)-1))

#vectors to record results 
ahrEPXPh = numeric(3)
ahrRFPh = numeric(3)
ahrEnsemblePh = numeric(3)

#start timer
startTime = Sys.time()

#run 3 replications of 10-fold CV
for (i in 1:3){
    #get phalanx formation on all the data
    phalanxes = getPhalanxes(dat = dat, seed = i, initialGroups = initialGroups)
    
    #obtain scores for Random Forest, EPX (phalanx built on training data), EPX (phalanx built on all data)
    score = cv(i = i, dat = dat, initialGroups = initialGroups, phalanxes = phalanxes)
    
    #record scores
    ahrEPXPh[i] = score[1]
    ahrRFPh[i] = score[2]
    ahrEnsemblePh[i] = score[3]
}

#end timer
endTime = Sys.time()
timePh = endTime - startTime














