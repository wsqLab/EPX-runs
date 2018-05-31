## Table showing corresponding R code with the columns in scores.csv
| Column | Code File Name |
| --- | --- |
| D | buildPhalanxBeforeCV.R |
| E | buildPhalanxBeforeRep.R |
| F|  compareAllMethods.R|
| G | compareAllMethods.R |
| H |  compareAllMethods.R  |

## Pseudo-code
compareAllMethods.R
<pre>
for run = 1:3
  obtain phalanx formation on whole data set

  for fold = 1:10
    get training and test set for current fold
    build ensembled Random Forest models on training set using phalanx formation
    build Random Forest model on the training set
    build the EPX model on the training set
    store prediction on test set on all 3 models separately
  end for  
    
  store average hit rate of each set of predictions separately 
end for
  
return the 3 set of average hit rate from the 3 runs
</pre>


buildPhalanxBeforeCV.R
<pre>
for run = 1:3
  for replication = 1:16
    obtain phalanx formation on whole data set

    for fold = 1:10
      get training and test set for current fold
      build ensembled Random Forest models on training set using phalanx formation
      store prediction on test set using ensembled Random Forest models
    end for  
    
    store average hit rate of the predictions
  end for
  
  store the mean average hit rate on the 16 replications
end for

return the 3 mean average hit rate from the 3 runs
</pre>

buildPhalanxBeforeRep.R
<pre>
for run = 1:3
  obtain phalanx formation on whole data set

  for replication = 1:16
    for fold = 1:10
      get training and test set for current fold
      build ensembled Random Forest models on training set using phalanx formation
      store prediction on test set using ensembled Random Forest models
    end for  
    
    store average hit rate of the predictions
  end for
  
  store the mean average hit rate on the 16 replications
end for

return the 3 mean average hit rate from the 3 runs
</pre>

