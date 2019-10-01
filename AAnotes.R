#Oct 1 2019

#check "realistic" movement speed from data (mean velocity probably fine)

#load E3 data, (see AUCshiftPlot.R for libraries and imports)

MV <- resultinfo3$pathlengthnorm/resultinfo3$movetime

plot(MV, resultinfo3$pathlengthnorm)

#divide absolute path length by MT for each trial, and take a peek at the distrubution.

#mean movement velocity is 0.076 m/sample (@100Hz)

#trajectoriesRealGeometry.R has the movement model with the actual size of the experimental workspace
#and the mean movement velocity coded in.

#play with parameters of the model to match data (make straighter lines, force occasional mistakes)

walks <- ranwalks(1024, 500, 1,1)

#get trajectories
trajs <- trajectories(walks, decbound = 1, suppresscount = TRUE)

#plot distribution of "decision time" - in this case time to reach target
hist(trajs$targetreached)

#find biggest
longestreach <- which(trajs$targetreached==max(trajs$targetreached))

sum(trajs$decision==1)

plot(trajs$effectorpos[[longestreach]][1,], trajs$effectorpos[[longestreach]][2,], type='l')
#decreasing decision boundary seems to straighten trajectories, but generate extreme cases that
#look a bit like model 3

#gridsearch decbound parameter from 1-200 to see if anything changes
decs <- numeric()
for (db in 51:200){
  trajd <- trajectories(walks, decbound = db, suppresscount = TRUE)
  decs <- append(decs, sum(trajd$decision==1))
}

#up to a decision boundary of 20, number of errors increases and asymptotes at 101

#then run some optimiser to get something reasonable

#goal is to have a model against which to compare submovement model

#if it's never going to work it will hopefully be obvious soon