#read data
dat = read.csv("~/AID348_Outcome/descriptors/BurdenNumbers.csv")
y = read.csv("~/AID348_Outcome/responses/Outcome.csv")
dat = dat[, 2:ncol(dat)]
y = as.integer(y[, 2])
dat$y = y

#get initial groupings for EPX model
getGroups = function(dat){
    groupNames = c()
    
    for (i in 1:(ncol(dat)-1)){
        groupNames = c(groupNames, strsplit(names(dat), "_")[[i]][1])
    }
    
    groupNames = unique(groupNames)
    
    if (length(groupNames) == 1){
        #case when dat = burden numbers
        return (c(1:(ncol(dat)-1)))
    }
    
    initialGroups = numeric(ncol(dat)-1)
    
    for (i in 1:(ncol(dat)-1)){
        initialGroups[i] = which(strsplit(names(dat), "_")[[i]][1] == groupNames)
    }
    
    return (initialGroups)
}

initialGroups = getGroups(dat)



