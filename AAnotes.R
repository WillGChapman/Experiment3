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
for (db in 1:50){
  walkd <- ranwalks(5000,500,1,1, suppress.count = TRUE)
  pb$tick()
  trajd <- trajectories(walkd, decbound = db, suppresscount = TRUE)
  decs <- append(decs, sum(trajd$decision==1))
}

plot(decs, type='l')

#up to a decision boundary of 20 (mean drift and variance of ), number of errors increases and asymptotes at 101 (with the same random walk dataset), which is interesting.
#perhaps this means that the decision variable is wrong for enough time that the incorrect destination is reached.
#the feedback gain on this probably helps.

#another gridsearch was run with ranwalks rerun every time. By about b=10, error rate starts to approach 10%
#then run some optimiser to get something reasonable

#trying again with a walks =5000, to see if we get a more stable estimate
#not much tbh.

#OK now - take a look at the trajectories generated. Keep the decision boundary at 10.

walks <- ranwalks(1000, 500, 1,1)

trajs <- trajectories(walks, decbound = 10, suppresscount = F)

#lets take a peek at the paths

plot(trajs$effectorpos[[1]][1,], trajs$effectorpos[[1]][2,], type='l', 
     col = alpha(colour = 'black', alpha = 0.25),
     xlim = c(-15,15))
for (trial in 1:length(trajs[[1]]))
{
  x <- 1
  if (trajs$decision[[trial]]==1){x = -1}
  lines(x*trajs$effectorpos[[trial]][1,], trajs$effectorpos[[trial]][2,], type='l', col = alpha(colour = 'black', alpha = 0.25))
}

#a higher decision boundary doesn't affect the rate at which incorrect decisions are made.

#goal is to have a model against which to compare submovement model

#if it's never going to work it will hopefully be obvious soon