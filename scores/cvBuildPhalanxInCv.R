#code for 10-fold CV where entirely new EPX model is built in each CV fold (ie for 3 replications of 10-fold CV, 30 EPX models are built)
library(EPX)
library(randomForest)
library(doParallel)

#create some helper functions
cv = function(seed, dat){
     #helper function for 10 fold cross validation
     #seed = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
     #dat = dataframe containing the data
     #returns average hit rate
     
     #randomly permute dataset but keep balanced CV
     set.seed(seed*100)
     s = c(sample(1:48), sample(49:nrow(dat))) 
     dat = dat[s, ]
     
     #vector of all CV predictions
     predAll = numeric(nrow(dat))
     predAllRF = numeric(nrow(dat))
     
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
          set.seed(seed*100+32)
          epxModel = epx(x = tr[, -ncol(tr)],
                         y = tr$y,
                         classifier.args = list(ntree = 500),
                         computing = "parallel")
          
          #unregister parallel
          parallel::stopCluster(cl)
          
          #compare with random forest
          set.seed(seed*100+32)
          rf = randomForest(as.factor(y) ~ ., tr, ntree = 500)
          
          #predict
          pred = predict(epxModel, newdata = te[, -ncol(te)])
          predRF = predict(rf, te, type = 'prob')[, 2]
          predAll[cvIndex == j] = pred
          predAllRF[cvIndex == j] = predRF
     }
     
     #returns average hit rate 
     return (c(AHR(y = dat$y, predAll), AHR(y = dat$y, predAllRF)))
}

ahrEPX = numeric(3)
ahrRF = numeric(3)

#start timer
startTime = Sys.time()

#run 3 replications of 10-fold CV
for (i in 1:3){
     score = cv(seed = i, dat = dat)
     ahrEPX = score[1]
     ahrRF = score[2]
}

endTime = Sys.time()
time = endTime - startTime











