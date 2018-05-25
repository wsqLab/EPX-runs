#code for 10-fold CV where entirely new EPX model is built in each CV fold (ie for 3 replications of 10-fold CV, 30 EPX models are built)
library(EPX)
library(randomForest)
library(doParallel)

#create some helper functions
cv = function(i, dat){
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
    
    #register cores for parallel computing
    clusters = parallel::detectCores()
    cl = parallel::makeCluster(clusters)
    doSNOW::registerDoSNOW(cl)
    
    #run cv in parallel
    scores = foreach (j = 1:10 %% 10, .combine = rbind, .packages = c("randomForest", "EPX")) %dopar% {
        #get training and test set
        cvIndex = c(1:nrow(dat)) %% 10
        tr = dat[cvIndex != j, ]
        te = dat[cvIndex == j, ]

        #build epx model
        set.seed(i*100+32)
        epxModel = epx(x = tr[, -ncol(tr)],
                       y = tr$y,
                       classifier.args = list(ntree = 500),
                       computing = "sequential")
        
        #compare with random forest
        set.seed(i*100+32)
        rfModel = randomForest(as.factor(y) ~ ., tr, ntree = 500)
        
        #predict
        predEPX = predict(epxModel, newdata = te[, -ncol(te)])
        predRF = predict(rfModel, te, type = 'prob')[, 2]

        #return results
        cbind(list(predEPX), list(predRF))
    }
    
    #unregister parallel
    parallel::stopCluster(cl)
    
    #store results in proper order (foreach does not return them in the order we want)
    cvIndex = c(1:nrow(dat)) %% 10
    for (j in 1:10){
        predAllEPX[cvIndex == (j %% 10)] = scores[j, 1][[1]]
        predAllRF[cvIndex == (j %% 10)] = scores[j, 2][[1]]
    }
    
    #returns average hit rate 
    return (c(AHR(y = dat$y, predAllEPX), AHR(y = dat$y, predAllRF)))
}

#run the CV
ahrEPX = numeric(3)
ahrRF = numeric(3)

#start timer
startTime = Sys.time()

#run 3 replications of 10-fold CV
for (i in 1:3){
    score = cv(i = i, dat = dat)
    ahrEPX[i] = score[1]
    ahrRF[i] = score[2]
}

endTime = Sys.time()
timePar = endTime - startTime

ahrEPX
ahrRF












