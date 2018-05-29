#helper functions for compareAllMethods script

getGroups = function(dat){
    #returns the initial groups using procedure outlined TomWelZam2015
    #dat = dataset 
    groupNames = c()
    
    for (i in 1:(ncol(dat)-1)){
        name = paste(strsplit(names(dat), "_")[[i]][1], strsplit(names(dat), "_")[[i]][3])
        groupNames = c(groupNames, name)
    }
    
    groupNames = unique(groupNames)
    
    if (length(groupNames) == 1){
        #case when dat = burden numbers
        return (c(1:(ncol(dat)-1)))
    }
    
    initialGroups = numeric(ncol(dat)-1)
    
    for (i in 1:(ncol(dat)-1)){
        name = paste(strsplit(names(dat), "_")[[i]][1], strsplit(names(dat), "_")[[i]][3])
        initialGroups[i] = which(groupNames == name)
    }
    
    return (initialGroups)
}

cv = function(i, dat, initialGroups = c(1:(ncol(dat)-1)), phalanxes){
    #helper function for 10 fold cross validation
    #i = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
    #dat = dataframe containing the data
    #returns average hit rate
    
    #randomly permute dataset but keep balanced CV
    set.seed(i*100)
    s = c(sample(1:48), sample(49:nrow(dat))) 
    dat = dat[s, ]
    
    #vector of all CV predictions
    predAllEPX = numeric(nrow(dat))
    predAllRF = numeric(nrow(dat))
    predAllEnsemble = numeric(nrow(dat))
    
    #register cores for parallel computing
    clusters = parallel::detectCores()
    cl = parallel::makeCluster(clusters)
    doSNOW::registerDoSNOW(cl)
    
    #run cv in parallel
    scores = foreach (j = 1:10 %% 10, 
                      .combine = rbind, 
                      .packages = c("randomForest", "EPX"),
                      .export = c("buildRFModels", "predictRFModels")
    ) %dopar% {
        #get training and test set
        cvIndex = c(1:nrow(dat)) %% 10
        tr = dat[cvIndex != j, ]
        te = dat[cvIndex == j, ]
        
        #build epx model
        set.seed(i*16+j+32)
        epxModel = epx(x = tr[, -ncol(tr)],
                       y = tr$y,
                       phalanxes.initial = initialGroups,
                       classifier.args = list(ntree = 500),
                       computing = "sequential")
        
        #compare with random forest
        set.seed(i*16+j+32)
        rfModel = randomForest(as.factor(y) ~ ., tr, ntree = 500)
        
        #build ensemble RF models
        set.seed(i*16+j+32)
        ensembleModel = buildRFModels(phalanxes = phalanxes, tr = tr, seed = i*16+j+32)
        
        #predict
        predEPX = predict(epxModel, newdata = te[, -ncol(te)])
        predRF = predict(rfModel, te, type = 'prob')[, 2]
        predEnsemble = predictRFModels(rfModels = ensembleModel, te = te)
        
        
        #return results
        cbind(list(predEPX), list(predRF), list(predEnsemble))
    }
    
    #unregister parallel
    parallel::stopCluster(cl)
    
    #store results in proper order (foreach does not return them in the order we want)
    cvIndex = c(1:nrow(dat)) %% 10
    for (j in 1:10){
        predAllEPX[cvIndex == (j %% 10)] = scores[j, 1][[1]]
        predAllRF[cvIndex == (j %% 10)] = scores[j, 2][[1]]
        predAllEnsemble[cvIndex == (j %% 10)] = scores[j, 3][[1]]
    }
    
    #returns average hit rate 
    return (c(AHR(y = dat$y, predAllEPX), AHR(y = dat$y, predAllRF), AHR(y = dat$y, predAllEnsemble)))
}

getPhalanxes = function(dat, seed, initialGroups = c(1:(ncol(dat)-1))){
    #dat = dataframe containing all the data
    #returns the phalanxes for building the EPX model
    
    #register cores for parallel computing
    #clusters = parallel::detectCores()
    #cl = parallel::makeCluster(clusters)
    #doSNOW::registerDoSNOW(cl)
    
    #build EPX model
    set.seed(101*seed+2)
    epxModel = epx(x = dat[, -ncol(dat)],
                   y = dat$y,
                   classifier.args = list(ntree = 500),
                   computing = "sequential",
                   phalanxes.initial = initialGroups)
    
    #unregister cores
    #parallel::stopCluster(cl)
    
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
