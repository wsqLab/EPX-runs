#code for performing CV where phalanxes are selected, then CV is performed
library(EPX)
library(randomForest)
library(doParallel)
source("~/RCode/git/buildPhalanxBeforeRepFunctions.R")

#track results
allAHR = numeric(16)
meanAHR = numeric(3)

#start timer
startTime = Sys.time()

#do 3 runs of 16 replications of 10-fold CV
for (i in 1:3){
    #get phalanx formation on all the data
    phalanxes = getPhalanxes(dat = dat, seed = (i-1)*16+j, initialGroups = initialGroups)
    
    #16 replications
    for (j in 1:16){
        #10-fold CV
        AHRAll[j] = cv(seed = (i-1)*16+j, dat = dat, phalanxes = phalanxes)
    }
    
    #average the AHR values obtained from each of the 16 runs
    meanAHR[i] = mean(AHRAll)
}

#end timer
endTime = Sys.time()
time = endTime - startTime

meanAHR
time













