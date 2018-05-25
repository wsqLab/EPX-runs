#compare runtime for different parallel computing methods for CV
library(EPX)
library(doParallel)
library(foreach)

#helper functions
cv = function(dat){
    #helper function for 10 fold cross validation
    #i = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
    #dat = dataframe containing the data
    #returns average hit rate
    
    #vector of all CV predictions
    predAll = numeric(nrow(dat))
    
    for (j in 0:9){
        #get training and test set
        cvIndex = c(1:nrow(dat)) %% 10
        tr = dat[cvIndex != j, ]
        te = dat[cvIndex == j, ]
        
        #register cores for parallel computing
        clusters = parallel::detectCores()
        cl = parallel::makeCluster(clusters)
        doSNOW::registerDoSNOW(cl)
        
        #build epx model
        set.seed(j*100+32)
        epxModel = epx(x = tr[, -ncol(tr)],
                       y = tr$y,
                       classifier.args = list(ntree = 500),
                       computing = "parallel")
        
        #unregister parallel
        parallel::stopCluster(cl)
        
        #predict
        pred = predict(epxModel, newdata = te[, -ncol(te)])
        predAll[cvIndex == j] = pred
    }
    
    #returns average hit rate 
    return (AHR(y = dat$y, predAll))
}

cvPar = function(dat){
    #helper function for 10 fold cross validation
    #i = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
    #dat = dataframe containing the data
    #returns average hit rate
    
    #vector of all CV predictions
    predAll = numeric(nrow(dat))
    
    #register cores
    clusters = parallel::detectCores()
    cl = parallel::makeCluster(clusters)
    doSNOW::registerDoSNOW(cl)
    
    #cv loop
    p = foreach(j = c(1:10) %% 10, .combine = c, .packages = 'EPX') %dopar% {
        #get training and test set
        cvIndex = c(1:nrow(dat)) %% 10
        tr = dat[cvIndex != j, ]
        te = dat[cvIndex == j, ]
        
        #build epx model
        set.seed(j*100+32)
        epxModel = epx(x = tr[, -ncol(tr)],
                       y = tr$y,
                       classifier.args = list(ntree = 500),
                       computing = "sequential")
        
        #predict
        pred = predict(epxModel, newdata = te[, -ncol(te)])
        predAll[cvIndex == j] = pred
    }
    
    #stop parallel
    parallel::stopCluster(cl)
    
    cvIndex = c(1:nrow(dat)) %% 10
    leftIndex = 1
    rightIndex = 0
    for (i in c(1:10) %% 10){
        n = sum(cvIndex == i)
        predAll[cvIndex == i] = p[leftIndex:(rightIndex + n)]
        leftIndex = rightIndex + n + 1
        rightIndex = rightIndex + n
    }

    #returns average hit rate 
    return (AHR(y = dat$y, predAll))
}

cvParMixed = function(dat){
    #helper function for 10 fold cross validation
    #i = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
    #dat = dataframe containing the data
    #returns average hit rate
    
    #vector of all CV predictions
    predAll = numeric(nrow(dat))
    
    #register cores
    clusters = 4
    cl = parallel::makeCluster(clusters)
    doSNOW::registerDoSNOW(cl)
    
    #cv loop
    p = foreach(j = c(1:10) %% 10, .combine = c, .packages = 'EPX') %dopar% {
        #get training and test set
        cvIndex = c(1:nrow(dat)) %% 10
        tr = dat[cvIndex != j, ]
        te = dat[cvIndex == j, ]
        
        #parallel
        clusters = 4
        cl2 = parallel::makeCluster(clusters)
        doSNOW::registerDoSNOW(cl2)
        
        #build epx model
        set.seed(j*100+32)
        epxModel = epx(x = tr[, -ncol(tr)],
                       y = tr$y,
                       classifier.args = list(ntree = 500),
                       computing = "parallel")
        
        #stop parallel
        parallel::stopCluster(c2)
        
        
        #predict
        pred = predict(epxModel, newdata = te[, -ncol(te)])
        predAll[cvIndex == j] = pred
    }
    
    #stop parallel
    parallel::stopCluster(cl)
    
    cvIndex = c(1:nrow(dat)) %% 10
    leftIndex = 1
    rightIndex = 0
    for (i in c(1:10) %% 10){
        n = sum(cvIndex == i)
        predAll[cvIndex == i] = p[leftIndex:(rightIndex + n)]
        leftIndex = rightIndex + n + 1
        rightIndex = rightIndex + n
    }
    
    #returns average hit rate 
    return (AHR(y = dat$y, predAll))
}


#take 1
#set up parallel computing
#start timer
startTime = Sys.time()

#build model
ahrEPXPar = cv(dat = dat)

#end timer
endTime = Sys.time()
epxParTime = endTime - startTime

#take2
#set up parallel computing
#start timer
startTime = Sys.time()

#build model
ahrCVPar = cvPar(dat = dat)

#end timer
endTime = Sys.time()
cvParTime = endTime - startTime


#take 3
#set up parallel computing
#start timer
startTime = Sys.time()

#build model
ahrCVParMixed = cvPar(dat = dat)

#end timer
endTime = Sys.time()
mixedParTime2 = endTime - startTime







