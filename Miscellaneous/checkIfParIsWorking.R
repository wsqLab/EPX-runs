source("~/RCode/git/Miscellaneous/compareAllMethodsFunctions.R")

cvSeq = function(i, dat, initialGroups = c(1:(ncol(dat)-1)), phalanxes){
     #helper function for 10 fold cross validation
     #i = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
     #dat = dataframe containing the data
     #returns average hit rate
     
     #randomly permute dataset but keep balanced CV
     set.seed(1)
     s = c(sample(1:48), sample(49:nrow(dat))) 
     dat = dat[s, ]
     
     #vector of all CV predictions
     predAllEPX = numeric(nrow(dat))
     predAllRF = numeric(nrow(dat))
     predAllEnsemble = numeric(nrow(dat))
     
     #run cv in parallel
     for (j in 1:10 %% 10){
          #get training and test set
          cvIndex = c(1:nrow(dat)) %% 10
          tr = dat[cvIndex != j, ]
          te = dat[cvIndex == j, ]
          
          #build epx model
          set.seed(1)
          epxModel = epx(x = tr[, -ncol(tr)],
                         y = tr$y,
                         phalanxes.initial = initialGroups,
                         classifier.args = list(ntree = 85),
                         computing = "sequential")
          
          #compare with random forest
          set.seed(1)
          rfModel = randomForest(as.factor(y) ~ ., tr, ntree = 85)
          
          #build ensemble RF models
          set.seed(1)
          ensembleModel = buildRFModels(phalanxes = phalanxes, tr = tr, seed = 1)
          
          #predict
          set.seed(1)
          predEPX = predict(epxModel, newdata = te[, -ncol(te)])
          
          predRF = predict(rfModel, te, type = 'prob')[, 2]
          predEnsemble = predictRFModels(rfModels = ensembleModel, te = te)
          
          #store predictions
          predAllEPX[cvIndex == j] = predEPX
          predAllRF[cvIndex == j] = predRF
          predAllEnsemble[cvIndex == j] = predEnsemble
     }
     
     #returns average hit rate 
     set.seed(1)
     return (c(AHR(y = dat$y, predAllEPX), AHR(y = dat$y, predAllRF), AHR(y = dat$y, predAllEnsemble)))
}

cvPar = function(i, dat, initialGroups = c(1:(ncol(dat)-1)), phalanxes){
     #helper function for 10 fold cross validation
     #i = the ith replication of CV (e.g. run 3 replications of 10-fold CV)
     #dat = dataframe containing the data
     #returns average hit rate
     
     #randomly permute dataset but keep balanced CV
     set.seed(1)
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
     ) %do% {
          #get training and test set
          cvIndex = c(1:nrow(dat)) %% 10
          tr = dat[cvIndex != j, ]
          te = dat[cvIndex == j, ]
          
          #build epx model
          set.seed(1)
          epxModel = epx(x = tr[, -ncol(tr)],
                         y = tr$y,
                         phalanxes.initial = initialGroups,
                         classifier.args = list(ntree = 85),
                         computing = "sequential")
          
          #compare with random forest
          set.seed(1)
          rfModel = randomForest(as.factor(y) ~ ., tr, ntree = 85)
          
          #build ensemble RF models
          set.seed(1)
          ensembleModel = buildRFModels(phalanxes = phalanxes, tr = tr, seed = 1)
          
          #predict
          set.seed(1)
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
     set.seed(1)
     return (c(AHR(y = dat$y, predAllEPX), AHR(y = dat$y, predAllRF), AHR(y = dat$y, predAllEnsemble)))
}

#test the results
i = 1
phalanxes = getPhalanxes(dat = dat, seed = i, initialGroups = initialGroups)
scoreSeq = cvSeq(i = i, dat = dat, initialGroups = initialGroups, phalanxes = phalanxes)
scorePar = cvPar(i = i, dat = dat, initialGroups = initialGroups, phalanxes = phalanxes)
print(scoreSeq == scorePar) #[1] TRUE  TRUE  TRUE







