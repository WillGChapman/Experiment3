
source("actionfocus.R")
source("modifyAUC.R")
#action focus plot

#get ranwalks
walks <- ranwalks(1024,500,1,1,F)

#make trajectories

decisionboundary <- 10

walks <- ranwalks(n_trials = 1000, n_time_samples = 500, drift_rate = 1, noise_sd = 1)

trajs <- trajectories(walks, decbound = decisionboundary)

#get which trajectories get the correct target
wdIndex <- which(trajs$decision==1)
cdIndex <- which(trajs$decision==2)

#get some specific trajectories
biggestwrongAUC <- wdIndex[which.max(trajs$AUC[wdIndex])]
biggestrightAUC <- cdIndex[which.max(trajs$AUC[cdIndex])]
medianrightAUC <- which(trajs$AUC==median(trajs$AUC[cdIndex]))

if(isempty(medianrightAUC)) medianrightAUC <- which(trajs$AUC==median(trajs$AUC[cdIndex[2:length(cdIndex)]]))

#get individual trajectory data for these:
layout(mat=1)

par(ask=TRUE)
for (gain in c(0,0.5,1,1.5,2,2.5,3,3.5,4))
{
bigright <- actionfocus(onewalk = walks[biggestrightAUC,], decbound = decisionboundary, model4 = TRUE, gain = gain, suppresscount = TRUE)
bigwrong <- actionfocus(onewalk = walks[biggestwrongAUC,], decbound = decisionboundary, model4 = TRUE, gain = gain, suppresscount = TRUE)
medright <- actionfocus(onewalk = walks[medianrightAUC,], decbound = decisionboundary, model4 = TRUE, gain = gain, suppresscount = TRUE)



#plot showing three trajectories

# model 4 algo

actfocusBR <- bigright$actionfocus
actfocusMR <- medright$actionfocus
actfocusBW <- bigwrong$actionfocus

time <- bigright$targetreached
time[2] <- bigwrong$targetreached
time[3] <- medright$targetreached

maxtime <- 5*ceiling(max(time)/5)


#panel 2
plot(bigright$zmod,
     xlab='Time',
     ylab='decision variable with commitment',
     ylim=c(-(decisionboundary+10),(decisionboundary+10)),
     xlim=c(0,maxtime),
     type = 'l',
     lwd=2.5,
     col=alpha('darkblue', 0.75))
abline(h = c(-decisionboundary,decisionboundary),lty='longdash')
lines(bigwrong$zmod,
      lwd=2.5,
      col=alpha('orange', 0.75))
lines(medright$zmod,
      lwd=2.5,
      col=alpha('purple', 0.75))
}
