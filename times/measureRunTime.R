#take 1
#set up parallel computing
clusters = parallel::detectCores()
cl = parallel::makeCluster(clusters)
doSNOW::registerDoSNOW(cl)

#start timer
startTime = Sys.time()

#build model
set.seed(123)
epx(x = dat[, -ncol(dat)], y = dat$y, classifier.args = list(ntree = 500), computing = "parallel")

#end timer
endTime = Sys.time()
parTime = endTime - startTime

#stop parallel
parallel::stopCluster(cl)


##time sequential
#start timer
startTime = Sys.time()

#build model
set.seed(123)
epx(x = dat[, -ncol(dat)], y = dat$y, classifier.args = list(ntree = 500), computing = "sequential")

#end timer
endTime = Sys.time()
timeSeq = endTime - startTime

#see the results
print(paste("Time for sequential computing:", timeSeq))
print(paste("Time for parallel computing method 1:", parTime))








