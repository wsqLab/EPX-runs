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
    predAll = numeric(nrow(dat))
    
    for (j in 0:9){
        print(paste(paste("j:", j), paste("i:", i), sep = ", "))
        
        #get training and test set
        cvIndex = c(1:nrow(dat)) %% 10
        tr = dat[cvIndex != j, ]
        te = dat[cvIndex == j, ]
        
        #register cores for parallel computing
        clusters = parallel::detectCores()
        cl = parallel::makeCluster(clusters)
        doSNOW::registerDoSNOW(cl)
        
        #build epx model
        set.seed(i*100+32)
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

AHRAll2 = numeric(3)
#start timer
startTime = Sys.time()

#run 3 replications of 10-fold CV
for (i in 1:3){
    AHRAll2[i] = cv(i = i, dat = dat)
}
endTime = Sys.time()
time = endTime - startTime

AHRAll2
time













