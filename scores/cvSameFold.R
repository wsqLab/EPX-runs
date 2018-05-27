#code for performing CV where phalanxes are selected, then CV is performed
library(EPX)
library(randomForest)
library(doParallel)

#create some helper funtions
getPhalanxes = function(dat, i){
    #dat = dataframe containing all the data
    #returns the phalanxes for building the EPX model
    
    #register cores for parallel computing
    clusters = parallel::detectCores()
    cl = parallel::makeCluster(clusters)
    doSNOW::registerDoSNOW(cl)
    
    #build EPX model
    set.seed(101*i+2)
    epxModel = epx(x = dat[, -ncol(dat)],
                   y = dat$y,
                   classifier.args = list(ntree = 500),
                   computing = "parallel")
    
    #unregister cores
    parallel::stopCluster(cl)
    
    #return the final phalanxes
    return (epxModel$PHALANXES$phalanxes.final)
}

buildRFModels = function(phAll, tr, i){
    #phAll = vector containing the information for which phalanx each variable belongs in
    #tr = data frame containing the data to train the RF models
    #builds the ensembled RF model given the data and phalanxes and returns the RF models in a list
    
    #create list of RF models
    rfModels = list()
    
    #iterate through phalanx and build the RF model in each case
    for (ph in unique(phAll)){
        trRF = tr[, c(phAll == ph, TRUE)]
        set.seed(i)
        rf = randomForest(as.factor(y) ~ ., trRF, ntree = 500)
        rfModels = append(rfModels, list(rf))
    }
    
    #returns list of RF models
    return (rfModels)
}

predictRFModels = function(rfModels, te){
    #returns prediction vector for the ensembled RF models
    #rfModels = list of RF models
    #te = test set
    
    #vector of predictions
    pred = numeric(nrow(te))
    
    for (rfModel in rfModels){
        pred = pred + predict(rfModel, te, type = 'prob')[, 2]
    }
    
    #returns ensembled predicted probabilities
    return (pred/length(rfModels))
}

cv = function(i, dat, phAll){
    #helper function for 10 fold cross validation
    #i = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
    #dat = dataframe containing the data
    #returns average hit rate
    
    #randomly permute dataset but keep balanced CV
    #set.seed(i*100 + 1)
    set.seed(i*100)
    s = c(sample(1:48), sample(49:nrow(dat))) 
    dat = dat[s, ]
    
    #vector of all CV predictions
    predAllPhOut = numeric(nrow(dat)) #prediction vector where phalanxes are built out of CV
    predAllPhIn = numeric(nrow(dat)) #prediction vector where phalanxes are built in CV
    
    
    for (j in 0:9){
        #get training and test set
        cvIndex = c(1:nrow(dat)) %% 10
        tr = dat[cvIndex != j, ]
        te = dat[cvIndex == j, ]
        
        #build epx model
        set.seed(i*100)
        model = buildRFModels(phAll = phAll, tr = tr, i = i)
        
        
        #register cores for parallel computing
        clusters = parallel::detectCores()
        cl = parallel::makeCluster(clusters)
        doSNOW::registerDoSNOW(cl)
        
        #build epx model
        epxModel = epx(x = tr[, -ncol(tr)],
                       y = tr$y,
                       classifier.args = list(ntree = 500),
                       computing = "parallel")
        
        #unregister parallel
        parallel::stopCluster(cl)
        
        #predict
        predPhOut = predictRFModels(rfModels = model, te = te)
        predPhIn = predict(epxModel, newdata = te[, -ncol(te)])
        predAllPhOut[cvIndex == j] = predPhOut
        predAllPhIn[cvIndex == j] = predPhIn
    }
    
    #returns average hit rate 
    return (c(AHR(y = dat$y, predAllPhOut), AHR(y = dat$y, predAllPhIn)))
}

#perform 3-fold CV
#track results
ahrPhOut = numeric(3)
ahrPhIn = numeric(3)

#start timer
startTime = Sys.time()

for (i in 1:3){
        phAll = getPhalanxes(dat = dat, i = i)
        scores = cv(i = i, dat = dat, phAll = phAll)
        ahrPhOut[i] = scores[1]
        ahrPhIn[i] = scores[2]
}

#end timer
endTime = Sys.time()
time = endTime - startTime

#see results
ahrPhOut
ahrPhIn




