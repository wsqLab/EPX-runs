#create some helper funtions for buildPhalanxBeforeCV

getPhalanxes = function(dat, seed, initialGroups = c(1:(ncol(dat)-1))){
    #dat = dataframe containing all the data
    #returns the phalanx formation for building the EPX model
    
    #register cores for parallel computing
    clusters = parallel::detectCores()
    cl = parallel::makeCluster(clusters)
    doSNOW::registerDoSNOW(cl)
    
    #build EPX model
    set.seed(101*seed+2)
    epxModel = epx(x = dat[, -ncol(dat)],
                   y = dat$y,
                   classifier.args = list(ntree = 500),
                   computing = "parallel",
                   phalanxes.initial = initialGroups)
    
    #unregister cores
    parallel::stopCluster(cl)
    
    #return the final phalanxes
    return (epxModel$PHALANXES$phalanxes.final)
}

buildRFModels = function(phalanxes, tr, seed){
    #phalanxes = vector containing the information for which phalanx each variable belongs in
    #tr = data frame containing the data to train the RF models
    #builds the ensembled RF model given the data and phalanxes and returns the RF models in a list
    
    #create list of RF models
    rfModels = list()
    
    #iterate through phalanx and build the RF model in each case
    for (ph in unique(phalanxes)){
        trRF = tr[, c(phalanxes == ph, TRUE)]
        set.seed(seed)
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
    
    #get predictions from all RF models
    for (rfModel in rfModels){
        pred = pred + predict(rfModel, te, type = 'prob')[, 2]
    }
    
    #returns ensembled predicted probabilities
    return (pred/length(rfModels))
}

cv = function(seed, dat, phalanxes){
    #helper function for 10 fold cross validation
    #i = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
    #dat = dataframe containing the data
    #returns average hit rate
    
    #randomly permute dataset but keep balanced CV
    #set.seed(i*100 + 1)
    set.seed(seed*100)
    s = c(sample(1:48), sample(49:nrow(dat))) 
    dat = dat[s, ]
    
    #vector of all CV predictions
    predAll = numeric(nrow(dat))
    
    for (j in 0:9){
        #get training and test set
        cvIndex = c(1:nrow(dat)) %% 10
        tr = dat[cvIndex != j, ]
        te = dat[cvIndex == j, ]
        
        #build epx model
        set.seed(i*100)
        model = buildRFModels(phalanxes = phalanxes, tr = tr, seed = seed)
        
        #predict
        pred = predictRFModels(rfModels = model, te = te)
        predAll[cvIndex == j] = pred
    }
    
    #returns average hit rate 
    return (AHR(y = dat$y, predAll))
}
